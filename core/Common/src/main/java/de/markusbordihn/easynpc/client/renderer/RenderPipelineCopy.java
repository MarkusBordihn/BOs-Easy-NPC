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

package de.markusbordihn.easynpc.client.renderer;

import com.mojang.renderpearl.api.pipeline.ColorTargetState;
import com.mojang.renderpearl.api.pipeline.RenderPipeline;
import com.mojang.renderpearl.api.vertex.VertexFormat;
import java.util.List;
import java.util.Optional;
import net.minecraft.resources.Identifier;

public final class RenderPipelineCopy {

  private RenderPipelineCopy() {}

  /**
   * Creates a pipeline builder with all states of the source pipeline, including shaders, shader
   * defines with their values, bind group layouts, color targets and vertex bindings.
   */
  public static RenderPipeline.Builder builder(RenderPipeline source, Identifier location) {
    List<ColorTargetState> colorTargetStates = source.getColorTargetStates();
    List<VertexFormat> vertexFormatBindings = source.getVertexFormatBindings();
    RenderPipeline.Snippet sourceSnippet =
        new RenderPipeline.Snippet(
            source.getShaders(),
            Optional.of(source.getShaderDefines()),
            Optional.of(source.getBindGroupLayouts()),
            colorTargetStates.toArray(new ColorTargetState[0]),
            colorTargetStates.size(),
            Optional.ofNullable(source.getDepthStencilState()),
            Optional.of(source.getPolygonMode()),
            Optional.of(source.isCull()),
            vertexFormatBindings.toArray(new VertexFormat[0]),
            Optional.of(source.getPrimitiveTopology()),
            source.pushConstantSize());
    return RenderPipeline.builder(sourceSnippet).withLocation(location);
  }
}
