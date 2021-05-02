/*
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
#include <string.h>

#include "libavutil/avassert.h"
#include "libavutil/mem.h"
#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"

#include "avfilter.h"
#include "framesync.h"
#include "formats.h"
#include "internal.h"
#include "vaapi_vpp.h"

typedef struct OverlayVAAPIContext {
    VAAPIVPPContext  vpp_ctx; // must be the first field
    FFFrameSync      fs;
    int              overlay_ox;
    int              overlay_oy;
    int              overlay_ow;
    int              overlay_oh;
    float            alpha;
} OverlayVAAPIContext;

static int overlay_vaapi_query_formats(AVFilterContext *ctx)
{
    int ret;
    static const enum AVPixelFormat fmts[] = {
        AV_PIX_FMT_VAAPI,
        AV_PIX_FMT_NONE
    };

    AVFilterFormats *pix_fmts = ff_make_format_list(fmts);

    return ff_set_common_formats(ctx, pix_fmts);
}

static int overlay_vaapi_build_filter_params(AVFilterContext *avctx)
{
    VAAPIVPPContext *vpp_ctx   = avctx->priv;
    VAStatus vas;
    int support_flag;
    VAProcPipelineCaps pipeline_caps;

    memset(&pipeline_caps, 0, sizeof(pipeline_caps));
    vas = vaQueryVideoProcPipelineCaps(vpp_ctx->hwctx->display,
                                       vpp_ctx->va_context,
                                       NULL, 0,
                                       &pipeline_caps);
    if (vas != VA_STATUS_SUCCESS) {
        av_log(avctx, AV_LOG_ERROR, "Failed to query pipeline "
               "caps: %d (%s).\n", vas, vaErrorStr(vas));
        return AVERROR(EIO);
    }

    if (!pipeline_caps.blend_flags) {
        av_log(avctx, AV_LOG_ERROR, "VAAPI driver doesn't support overlay\n");
        return AVERROR(EINVAL);
    }

    support_flag = pipeline_caps.blend_flags & VA_BLEND_GLOBAL_ALPHA;
    if (!support_flag) {
        av_log(avctx, AV_LOG_ERROR, "VAAPI driver doesn't support global alpha blending\n");
        return AVERROR(EINVAL);
    }

    return 0;
}

static int overlay_vaapi_blend(FFFrameSync *fs)
{
    AVFilterContext    *avctx = fs->parent;
    AVFilterLink     *outlink = avctx->outputs[0];
    OverlayVAAPIContext *ctx  = avctx->priv;
    VAAPIVPPContext *vpp_ctx  = avctx->priv;
    AVFrame *input_main, *input_overlay;
    AVFrame *output;
    VAProcPipelineParameterBuffer params;
    VABlendState blend_state; // Blend State
    VARectangle overlay_region, output_region;
    int err;

    err = overlay_vaapi_build_filter_params(avctx);
    if (err < 0)
        return err;

    err = ff_framesync_dualinput_get(fs, &input_main, &input_overlay);
    if (err < 0)
        return err;
    av_log(avctx, AV_LOG_DEBUG, "Filter main: %s, %ux%u (%"PRId64").\n",
           av_get_pix_fmt_name(input_main->format),
           input_main->width, input_main->height, input_main->pts);

    av_log(avctx, AV_LOG_DEBUG, "Filter overlay: %s, %ux%u (%"PRId64").\n",
           av_get_pix_fmt_name(input_overlay->format),
           input_overlay->width, input_overlay->height, input_overlay->pts);

    if (vpp_ctx->va_context == VA_INVALID_ID) {
        err = AVERROR(EINVAL);
        goto fail;
    }

    err = av_frame_make_writable(input_main);
    if (err < 0)
        goto fail;
    output = input_main;
    err = ff_vaapi_vpp_init_params(avctx, &params,
                                   input_overlay, output);
    if (err < 0)
        goto fail;

    overlay_region = (VARectangle) {
        .x      = ctx->overlay_ox,
        .y      = ctx->overlay_oy,
        .width  = ctx->overlay_ow ? ctx->overlay_ow : input_overlay->width,
        .height = ctx->overlay_oh ? ctx->overlay_oh : input_overlay->height,
    };

    if (overlay_region.x + overlay_region.width > input_main->width ||
        overlay_region.y + overlay_region.height > input_main->height) {
        av_log(ctx, AV_LOG_WARNING,
               "The overlay image exceeds the scope of the main image, "
               "will crop the overlay image according based on the main image.\n");
    }

    params.filters     = &vpp_ctx->filter_buffers[0];
    params.num_filters = vpp_ctx->nb_filter_buffers;

    params.output_region = &overlay_region;
    //params.output_background_color = VAAPI_VPP_BACKGROUND_BLACK;

    //blend_state.flags = VA_BLEND_GLOBAL_ALPHA;
    //blend_state.global_alpha = ctx->alpha;
{
    static int flag = 1;
    flag = !flag;
    params.output_background_color = flag == 0 ? 0x0 : 0xffff0000;
    params.filter_flags = VA_FILTER_SCALING_HQ;

    err = ff_vaapi_vpp_render_picture(avctx, &params, output);
    if (err < 0)
        goto fail;

}
    av_log(avctx, AV_LOG_DEBUG, "Filter output: %s, %ux%u (%"PRId64").\n",
           av_get_pix_fmt_name(output->format),
           output->width, output->height, output->pts);
    return ff_filter_frame(outlink, output);

fail:
    av_frame_free(&input_main);
    return err;
}

static int overlay_vaapi_init_framesync(AVFilterContext *avctx)
{
    OverlayVAAPIContext *ctx = avctx->priv;
    int ret, i;

    ctx->fs.on_event = overlay_vaapi_blend;
    ctx->fs.opaque   = ctx;
    ret = ff_framesync_init(&ctx->fs, avctx, avctx->nb_inputs);
    if (ret < 0)
        return ret;

    for (i = 0; i < avctx->nb_inputs; i++) {
        FFFrameSyncIn *in = &ctx->fs.in[i];
        in->before    = EXT_STOP;
        in->after     = EXT_INFINITY;
        in->sync      = i ? 1 : 2;
        in->time_base = avctx->inputs[i]->time_base;
    }

    return ff_framesync_configure(&ctx->fs);
}

static int overlay_vaapi_config_output(AVFilterLink *outlink)
{
    int err;
    AVFilterContext  *avctx  = outlink->src;
    OverlayVAAPIContext *ctx = avctx->priv;
    VAAPIVPPContext *vpp_ctx = avctx->priv;
    AVFilterLink *inlink = avctx->inputs[0];
    AVHWFramesContext  *frames_ctx = (AVHWFramesContext*)inlink->hw_frames_ctx->data;

    AVFilterLink *inlink_overlay = avctx->inputs[1];
    AVHWFramesContext  *frames_ctx_overlay = (AVHWFramesContext*)inlink_overlay->hw_frames_ctx->data;

    if (!frames_ctx) {
        av_log(ctx, AV_LOG_ERROR, "No hw context provided on main input\n");
        return AVERROR(EINVAL);
    }

    if (!frames_ctx_overlay) {
        av_log(ctx, AV_LOG_ERROR, "No hw context provided on overlay input\n");
        return AVERROR(EINVAL);
    }

    err = overlay_vaapi_init_framesync(avctx);
    if (err < 0)
        return err;

    vpp_ctx->input_frames_ref = av_buffer_ref(inlink->hw_frames_ctx);
    if (!vpp_ctx->input_frames_ref) {
        av_log(avctx, AV_LOG_ERROR, "A input frames reference create "
            "failed.\n");
        return AVERROR(ENOMEM);
    }
    vpp_ctx->input_frames = (AVHWFramesContext*)vpp_ctx->input_frames_ref->data;
    vpp_ctx->reuse_input_frames = 1;

    err = ff_vaapi_vpp_config_output(outlink);
    if (err < 0)
        return err;

    err = ff_framesync_init_dualinput(&ctx->fs, avctx);
    if (err < 0)
        return err;

    return ff_framesync_configure(&ctx->fs);
}

static av_cold int overlay_vaapi_init(AVFilterContext *avctx)
{
    VAAPIVPPContext *vpp_ctx = avctx->priv;

    ff_vaapi_vpp_ctx_init(avctx);
    vpp_ctx->output_format = AV_PIX_FMT_NONE;

    return 0;
}

static int overlay_vaapi_activate(AVFilterContext *avctx)
{
    OverlayVAAPIContext *ctx = avctx->priv;

    return ff_framesync_activate(&ctx->fs);
}

static av_cold void overlay_vaapi_uninit(AVFilterContext *avctx)
{
    OverlayVAAPIContext *ctx = avctx->priv;

    ff_framesync_uninit(&ctx->fs);
}

#define OFFSET(x) offsetof(OverlayVAAPIContext, x)
#define FLAGS (AV_OPT_FLAG_FILTERING_PARAM | AV_OPT_FLAG_VIDEO_PARAM)
static const AVOption overlay_vaapi_options[] = {
    { "x", "Overlay x position",
      OFFSET(overlay_ox), AV_OPT_TYPE_INT, { .i64 = 0 }, 0, INT_MAX, .flags = FLAGS },
    { "y", "Overlay y position",
      OFFSET(overlay_oy), AV_OPT_TYPE_INT, { .i64 = 0 }, 0, INT_MAX, .flags = FLAGS },
    { "w", "Overlay width",
      OFFSET(overlay_ow), AV_OPT_TYPE_INT, { .i64 = 0 }, 0, INT_MAX, .flags = FLAGS },
    { "h", "Overlay height",
      OFFSET(overlay_oh), AV_OPT_TYPE_INT, { .i64 = 0 }, 0, INT_MAX, .flags = FLAGS },
    { "alpha", "Overlay global alpha",
      OFFSET(alpha), AV_OPT_TYPE_FLOAT, { .dbl = 1.0}, 0.0, 1.0, .flags = FLAGS},
    { "eof_action", "Action to take when encountering EOF from secondary input ",
        OFFSET(fs.opt_eof_action), AV_OPT_TYPE_INT, { .i64 = EOF_ACTION_REPEAT },
        EOF_ACTION_REPEAT, EOF_ACTION_PASS, .flags = FLAGS, "eof_action" },
        { "repeat", "Repeat the previous frame.",   0, AV_OPT_TYPE_CONST, { .i64 = EOF_ACTION_REPEAT }, .flags = FLAGS, "eof_action" },
        { "endall", "End both streams.",            0, AV_OPT_TYPE_CONST, { .i64 = EOF_ACTION_ENDALL }, .flags = FLAGS, "eof_action" },
        { "pass",   "Pass through the main input.", 0, AV_OPT_TYPE_CONST, { .i64 = EOF_ACTION_PASS },   .flags = FLAGS, "eof_action" },
    { "shortest", "force termination when the shortest input terminates", OFFSET(fs.opt_shortest), AV_OPT_TYPE_BOOL, { .i64 = 0 }, 0, 1, FLAGS },
    { "repeatlast", "repeat overlay of the last overlay frame", OFFSET(fs.opt_repeatlast), AV_OPT_TYPE_BOOL, {.i64=1}, 0, 1, FLAGS },
    { NULL },
};

AVFILTER_DEFINE_CLASS(overlay_vaapi);

static const AVFilterPad overlay_vaapi_inputs[] = {
    {
        .name             = "main",
        .type             = AVMEDIA_TYPE_VIDEO,
    },
    {
        .name             = "overlay",
        .type             = AVMEDIA_TYPE_VIDEO,
    },
    { NULL }
};

static const AVFilterPad overlay_vaapi_outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
        .config_props  = &overlay_vaapi_config_output,
    },
    { NULL }
};

AVFilter ff_vf_overlay_vaapi = {
    .name            = "overlay_vaapi",
    .description     = NULL_IF_CONFIG_SMALL("Overlay one video on top of another"),
    .priv_size       = sizeof(OverlayVAAPIContext),
    .priv_class      = &overlay_vaapi_class,
    .init            = &overlay_vaapi_init,
    .uninit          = &overlay_vaapi_uninit,
    .query_formats   = &overlay_vaapi_query_formats,
    .activate        = &overlay_vaapi_activate,
    .inputs          = overlay_vaapi_inputs,
    .outputs         = overlay_vaapi_outputs,
    .flags_internal  = FF_FILTER_FLAG_HWFRAME_AWARE,
};
