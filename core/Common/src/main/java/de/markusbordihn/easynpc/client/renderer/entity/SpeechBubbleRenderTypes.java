/*
 * Copyright 2026 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.client.renderer.entity;

import com.mojang.blaze3d.pipeline.RenderPipeline;
import com.mojang.blaze3d.platform.DepthTestFunction;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.renderer.RenderTypeTextureAccessor;
import de.markusbordihn.easynpc.compat.iris.IrisManager;
import java.util.Optional;
import net.minecraft.client.renderer.RenderPipelines;
import net.minecraft.client.renderer.rendertype.RenderSetup;
import net.minecraft.client.renderer.rendertype.RenderType;
import net.minecraft.resources.Identifier;

public final class SpeechBubbleRenderTypes {

  private static final RenderPipeline OCCLUDED_PIPELINE = createOccludedPipeline();
  private static final boolean SHADER_MAPPING_AVAILABLE =
      IrisManager.copyShaderMapping(RenderPipelines.TEXT, OCCLUDED_PIPELINE);
  private static final RenderType OCCLUDED =
      RenderType.create(
          Constants.MOD_PREFIX_ID + "speech_bubble_occluded",
          RenderSetup.builder(OCCLUDED_PIPELINE)
              .withTexture(
                  RenderTypeTextureAccessor.PRIMARY_TEXTURE_SAMPLER,
                  Constants.TEXTURE_SPEECH_BUBBLE)
              .useLightmap()
              .bufferSize(RenderType.SMALL_BUFFER_SIZE)
              .createRenderSetup());

  private SpeechBubbleRenderTypes() {}

  public static boolean isShaderMappingAvailable() {
    return SHADER_MAPPING_AVAILABLE;
  }

  public static RenderType occluded() {
    return OCCLUDED;
  }

  private static RenderPipeline createOccludedPipeline() {
    RenderPipeline textPipeline = RenderPipelines.TEXT;
    return RenderPipeline.builder(snippetOf(textPipeline))
        .withLocation(
            Identifier.fromNamespaceAndPath(Constants.MOD_ID, "pipeline/speech_bubble_occluded"))
        .withDepthTestFunction(DepthTestFunction.GREATER_DEPTH_TEST)
        .withDepthWrite(false)
        .withDepthBias(textPipeline.getDepthBiasScaleFactor(), textPipeline.getDepthBiasConstant())
        .build();
  }

  private static RenderPipeline.Snippet snippetOf(RenderPipeline renderPipeline) {
    return new RenderPipeline.Snippet(
        Optional.of(renderPipeline.getVertexShader()),
        Optional.of(renderPipeline.getFragmentShader()),
        Optional.of(renderPipeline.getShaderDefines()),
        Optional.of(renderPipeline.getSamplers()),
        Optional.of(renderPipeline.getUniforms()),
        renderPipeline.getBlendFunction(),
        Optional.of(renderPipeline.getDepthTestFunction()),
        Optional.of(renderPipeline.getPolygonMode()),
        Optional.of(renderPipeline.isCull()),
        Optional.of(renderPipeline.isWriteColor()),
        Optional.of(renderPipeline.isWriteAlpha()),
        Optional.of(renderPipeline.isWriteDepth()),
        Optional.of(renderPipeline.getColorLogic()),
        Optional.of(renderPipeline.getVertexFormat()),
        Optional.of(renderPipeline.getVertexFormatMode()));
  }
}
