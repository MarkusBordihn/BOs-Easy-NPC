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

import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import com.mojang.blaze3d.vertex.VertexConsumer;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Optional;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;

public class OpacityBufferSource implements MultiBufferSource {

  private static final Map<RenderType, RenderType> TRANSLUCENT_RENDER_TYPES =
      new IdentityHashMap<>();

  private final MultiBufferSource bufferSource;
  private final float alpha;
  private final Map<VertexConsumer, OpacityVertexConsumer> vertexConsumers =
      new IdentityHashMap<>();

  private OpacityBufferSource(MultiBufferSource bufferSource, float alpha) {
    this.bufferSource = bufferSource;
    this.alpha = alpha;
  }

  public static MultiBufferSource wrapIfNeeded(Entity entity, MultiBufferSource bufferSource) {
    if (bufferSource == null
        || bufferSource instanceof OpacityBufferSource
        || !(entity instanceof EasyNPC<?> easyNPC)) {
      return bufferSource;
    }

    int opacity = AttributeHandler.getOpacity(easyNPC);
    if (opacity >= DisplayAttributeType.MAX_OPACITY) {
      return bufferSource;
    }

    return new OpacityBufferSource(
        bufferSource, opacity / (float) DisplayAttributeType.MAX_OPACITY);
  }

  private static boolean hasColorElement(RenderType renderType) {
    return renderType.format() == DefaultVertexFormat.NEW_ENTITY;
  }

  private static RenderType toTranslucentRenderType(RenderType renderType) {
    Optional<ResourceLocation> texture =
        renderType instanceof RenderTypeTextureAccessor textureAccessor
            ? textureAccessor.easyNPC$getTexture()
            : Optional.empty();
    if (texture.isEmpty()) {
      return renderType;
    }

    ResourceLocation textureLocation = texture.get();
    if (renderType == RenderType.entitySolid(textureLocation)
        || renderType == RenderType.entityCutout(textureLocation)
        || renderType == RenderType.entitySmoothCutout(textureLocation)) {
      return RenderType.entityTranslucentCull(textureLocation);
    }
    if (renderType == RenderType.entityCutoutNoCull(textureLocation)
        || renderType == RenderType.entityCutoutNoCullZOffset(textureLocation)
        || renderType == RenderType.armorCutoutNoCull(textureLocation)) {
      return RenderType.entityTranslucent(textureLocation);
    }

    return renderType;
  }

  @Override
  public VertexConsumer getBuffer(RenderType renderType) {
    if (!hasColorElement(renderType)) {
      return this.bufferSource.getBuffer(renderType);
    }

    VertexConsumer vertexConsumer =
        this.bufferSource.getBuffer(
            TRANSLUCENT_RENDER_TYPES.computeIfAbsent(
                renderType, OpacityBufferSource::toTranslucentRenderType));
    return this.vertexConsumers.computeIfAbsent(
        vertexConsumer, delegate -> new OpacityVertexConsumer(delegate, this.alpha));
  }
}
