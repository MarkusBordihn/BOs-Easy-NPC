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

import com.mojang.blaze3d.pipeline.BindGroupLayout;
import com.mojang.blaze3d.pipeline.ColorTargetState;
import com.mojang.blaze3d.pipeline.DepthStencilState;
import com.mojang.blaze3d.pipeline.RenderPipeline;
import com.mojang.blaze3d.platform.CompareOp;
import com.mojang.blaze3d.vertex.VertexFormat;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.renderer.RenderTypeTextureAccessor;
import de.markusbordihn.easynpc.compat.iris.IrisManager;
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
    RenderPipeline.Builder builder =
        RenderPipeline.builder()
            .withLocation(
                Identifier.fromNamespaceAndPath(
                    Constants.MOD_ID, "pipeline/speech_bubble_occluded"))
            .withVertexShader(textPipeline.getVertexShader())
            .withFragmentShader(textPipeline.getFragmentShader())
            .withPolygonMode(textPipeline.getPolygonMode())
            .withCull(textPipeline.isCull())
            .withPrimitiveTopology(textPipeline.getPrimitiveTopology())
            .withDepthStencilState(new DepthStencilState(CompareOp.LESS_THAN, false));

    for (String shaderDefine : textPipeline.getShaderDefines().flags()) {
      builder.withShaderDefine(shaderDefine);
    }
    for (BindGroupLayout bindGroupLayout : textPipeline.getBindGroupLayouts()) {
      builder.withBindGroupLayout(bindGroupLayout);
    }

    ColorTargetState[] colorTargetStates = textPipeline.getColorTargetStates();
    for (int index = 0; index < colorTargetStates.length; index++) {
      if (colorTargetStates[index] == null) {
        builder.withUnusedColorTargetState(index);
      } else {
        builder.withColorTargetState(index, colorTargetStates[index]);
      }
    }

    VertexFormat[] vertexFormatBindings = textPipeline.getVertexFormatBindings();
    for (int index = 0; index < vertexFormatBindings.length; index++) {
      builder.withVertexBinding(index, vertexFormatBindings[index]);
    }

    return builder.build();
  }
}
