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

import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import com.mojang.blaze3d.vertex.VertexFormat;
import de.markusbordihn.easynpc.Constants;
import java.util.IdentityHashMap;
import java.util.Map;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderStateShard;
import net.minecraft.client.renderer.RenderType;

public class SpeechBubbleRenderTypes extends RenderType {

  private static final int BUFFER_SIZE = 256;

  private static final Map<RenderType, RenderType> opaqueTextRenderTypes = new IdentityHashMap<>();

  private static final RenderStateShard.TextureStateShard SPEECH_BUBBLE_TEXTURE =
      new RenderStateShard.TextureStateShard(Constants.TEXTURE_SPEECH_BUBBLE, false, false);

  private static final RenderType OCCLUDED =
      new SpeechBubbleRenderTypes(
          Constants.MOD_ID + ":speech_bubble_occluded",
          DefaultVertexFormat.POSITION_COLOR_TEX_LIGHTMAP,
          VertexFormat.Mode.QUADS,
          BUFFER_SIZE,
          false,
          true,
          () -> {
            SPEECH_BUBBLE_TEXTURE.setupRenderState();
            RENDERTYPE_TEXT_SHADER.setupRenderState();
            TRANSLUCENT_TRANSPARENCY.setupRenderState();
            GREATER_DEPTH_TEST.setupRenderState();
            LIGHTMAP.setupRenderState();
            COLOR_WRITE.setupRenderState();
          },
          () -> {
            COLOR_WRITE.clearRenderState();
            LIGHTMAP.clearRenderState();
            GREATER_DEPTH_TEST.clearRenderState();
            TRANSLUCENT_TRANSPARENCY.clearRenderState();
            RENDERTYPE_TEXT_SHADER.clearRenderState();
            SPEECH_BUBBLE_TEXTURE.clearRenderState();
          });

  private static final RenderType OPAQUE_BODY =
      new SpeechBubbleRenderTypes(
          Constants.MOD_ID + ":speech_bubble_opaque",
          DefaultVertexFormat.POSITION_COLOR_TEX_LIGHTMAP,
          VertexFormat.Mode.QUADS,
          BUFFER_SIZE,
          false,
          false,
          () -> {
            SPEECH_BUBBLE_TEXTURE.setupRenderState();
            RENDERTYPE_TEXT_SHADER.setupRenderState();
            NO_TRANSPARENCY.setupRenderState();
            LEQUAL_DEPTH_TEST.setupRenderState();
            LIGHTMAP.setupRenderState();
          },
          () -> {
            LIGHTMAP.clearRenderState();
            LEQUAL_DEPTH_TEST.clearRenderState();
            NO_TRANSPARENCY.clearRenderState();
            RENDERTYPE_TEXT_SHADER.clearRenderState();
            SPEECH_BUBBLE_TEXTURE.clearRenderState();
          });

  private SpeechBubbleRenderTypes(
      String name,
      VertexFormat format,
      VertexFormat.Mode mode,
      int bufferSize,
      boolean affectsCrumbling,
      boolean sortOnUpload,
      Runnable setupState,
      Runnable clearState) {
    super(name, format, mode, bufferSize, affectsCrumbling, sortOnUpload, setupState, clearState);
  }

  public static RenderType occluded() {
    return OCCLUDED;
  }

  public static RenderType opaqueBody() {
    return OPAQUE_BODY;
  }

  public static MultiBufferSource opaqueTextBufferSource(MultiBufferSource bufferSource) {
    return renderType -> bufferSource.getBuffer(opaqueText(renderType));
  }

  private static RenderType opaqueText(RenderType textRenderType) {
    return opaqueTextRenderTypes.computeIfAbsent(
        textRenderType,
        sourceRenderType ->
            new SpeechBubbleRenderTypes(
                Constants.MOD_ID + ":speech_bubble_opaque_text/" + sourceRenderType,
                sourceRenderType.format(),
                sourceRenderType.mode(),
                BUFFER_SIZE,
                false,
                false,
                () -> {
                  sourceRenderType.setupRenderState();
                  NO_TRANSPARENCY.setupRenderState();
                },
                () -> {
                  NO_TRANSPARENCY.clearRenderState();
                  sourceRenderType.clearRenderState();
                }));
  }
}
