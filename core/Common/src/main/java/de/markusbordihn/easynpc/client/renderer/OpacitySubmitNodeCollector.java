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

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexFormatElement;
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.client.gui.Font;
import net.minecraft.client.model.Model;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.renderer.OrderedSubmitNodeCollector;
import net.minecraft.client.renderer.SubmitNodeCollector;
import net.minecraft.client.renderer.block.MovingBlockRenderState;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.renderer.block.model.BlockStateModel;
import net.minecraft.client.renderer.entity.state.EntityRenderState;
import net.minecraft.client.renderer.feature.ModelFeatureRenderer;
import net.minecraft.client.renderer.item.ItemStackRenderState;
import net.minecraft.client.renderer.rendertype.RenderType;
import net.minecraft.client.renderer.rendertype.RenderTypes;
import net.minecraft.client.renderer.state.CameraRenderState;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.Identifier;
import net.minecraft.util.ARGB;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.item.ItemDisplayContext;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.Vec3;
import org.joml.Quaternionf;

public class OpacitySubmitNodeCollector implements SubmitNodeCollector {

  private static final Map<RenderType, RenderType> TRANSLUCENT_RENDER_TYPES =
      new IdentityHashMap<>();

  private final OrderedSubmitNodeCollector collector;
  private final float alpha;

  private OpacitySubmitNodeCollector(OrderedSubmitNodeCollector collector, float alpha) {
    this.collector = collector;
    this.alpha = alpha;
  }

  public static SubmitNodeCollector wrapIfNeeded(
      EasyNPC<?> easyNPC, SubmitNodeCollector submitNodeCollector) {
    if (easyNPC == null
        || submitNodeCollector == null
        || submitNodeCollector instanceof OpacitySubmitNodeCollector) {
      return submitNodeCollector;
    }

    int opacity = AttributeHandler.getOpacity(easyNPC);
    if (opacity >= DisplayAttributeType.MAX_OPACITY) {
      return submitNodeCollector;
    }

    return new OpacitySubmitNodeCollector(
        submitNodeCollector, opacity / (float) DisplayAttributeType.MAX_OPACITY);
  }

  public static SubmitNodeCollector wrapIfNeeded(
      Entity entity, SubmitNodeCollector submitNodeCollector) {
    return entity instanceof EasyNPC<?> easyNPC
        ? wrapIfNeeded(easyNPC, submitNodeCollector)
        : submitNodeCollector;
  }

  public static SubmitNodeCollector wrapIfNeeded(
      EntityRenderState renderState, SubmitNodeCollector submitNodeCollector) {
    if (!(renderState instanceof EasyNPCRenderStateExtension renderStateExtension)) {
      return submitNodeCollector;
    }

    UUID uuid = renderStateExtension.getEasyNpcUUID();
    return uuid != null
        ? wrapIfNeeded(LivingEntityManager.getClientEasyNPCEntityByUUID(uuid), submitNodeCollector)
        : submitNodeCollector;
  }

  private static boolean hasColorElement(RenderType renderType) {
    return renderType.format().contains(VertexFormatElement.COLOR);
  }

  private static RenderType toTranslucentRenderType(RenderType renderType) {
    Optional<Identifier> texture =
        renderType instanceof RenderTypeTextureAccessor textureAccessor
            ? textureAccessor.easyNPC$getTexture()
            : Optional.empty();
    if (texture.isEmpty()) {
      return renderType;
    }

    Identifier textureLocation = texture.get();
    if (renderType == RenderTypes.armorCutoutNoCull(textureLocation)) {
      return RenderTypes.armorTranslucent(textureLocation);
    }

    if (renderType == RenderTypes.entitySolid(textureLocation)
        || renderType == RenderTypes.entityCutout(textureLocation)
        || renderType == RenderTypes.entitySmoothCutout(textureLocation)
        || renderType == RenderTypes.entityCutoutNoCull(textureLocation)
        || renderType == RenderTypes.entityCutoutNoCullZOffset(textureLocation)) {
      return RenderTypes.entityTranslucent(textureLocation);
    }

    return renderType;
  }

  private RenderType translucentRenderType(RenderType renderType) {
    return TRANSLUCENT_RENDER_TYPES.computeIfAbsent(
        renderType, OpacitySubmitNodeCollector::toTranslucentRenderType);
  }

  private int[] fadeTintLayers(int[] tintLayers) {
    int[] fadedTintLayers = new int[tintLayers.length];
    for (int index = 0; index < tintLayers.length; index++) {
      fadedTintLayers[index] = ARGB.multiplyAlpha(tintLayers[index], this.alpha);
    }

    return fadedTintLayers;
  }

  @Override
  public OrderedSubmitNodeCollector order(int order) {
    return this.collector instanceof SubmitNodeCollector submitNodeCollector
        ? new OpacitySubmitNodeCollector(submitNodeCollector.order(order), this.alpha)
        : this;
  }

  @Override
  public <S> void submitModel(
      Model<? super S> model,
      S state,
      PoseStack poseStack,
      RenderType renderType,
      int packedLight,
      int packedOverlay,
      int color,
      TextureAtlasSprite textureAtlasSprite,
      int outlineColor,
      ModelFeatureRenderer.CrumblingOverlay crumblingOverlay) {
    if (!hasColorElement(renderType)) {
      this.collector.submitModel(
          model,
          state,
          poseStack,
          renderType,
          packedLight,
          packedOverlay,
          color,
          textureAtlasSprite,
          outlineColor,
          crumblingOverlay);
      return;
    }

    this.collector.submitModel(
        model,
        state,
        poseStack,
        this.translucentRenderType(renderType),
        packedLight,
        packedOverlay,
        ARGB.multiplyAlpha(color, this.alpha),
        textureAtlasSprite,
        outlineColor,
        crumblingOverlay);
  }

  @Override
  public void submitModelPart(
      ModelPart modelPart,
      PoseStack poseStack,
      RenderType renderType,
      int packedLight,
      int packedOverlay,
      TextureAtlasSprite textureAtlasSprite,
      boolean visible,
      boolean skipDraw,
      int color,
      ModelFeatureRenderer.CrumblingOverlay crumblingOverlay,
      int outlineColor) {
    if (!hasColorElement(renderType)) {
      this.collector.submitModelPart(
          modelPart,
          poseStack,
          renderType,
          packedLight,
          packedOverlay,
          textureAtlasSprite,
          visible,
          skipDraw,
          color,
          crumblingOverlay,
          outlineColor);
      return;
    }

    this.collector.submitModelPart(
        modelPart,
        poseStack,
        this.translucentRenderType(renderType),
        packedLight,
        packedOverlay,
        textureAtlasSprite,
        visible,
        skipDraw,
        ARGB.multiplyAlpha(color, this.alpha),
        crumblingOverlay,
        outlineColor);
  }

  @Override
  public void submitItem(
      PoseStack poseStack,
      ItemDisplayContext itemDisplayContext,
      int packedLight,
      int packedOverlay,
      int outlineColor,
      int[] tintLayers,
      List<BakedQuad> quads,
      RenderType renderType,
      ItemStackRenderState.FoilType foilType) {
    if (!hasColorElement(renderType)) {
      this.collector.submitItem(
          poseStack,
          itemDisplayContext,
          packedLight,
          packedOverlay,
          outlineColor,
          tintLayers,
          quads,
          renderType,
          foilType);
      return;
    }

    this.collector.submitItem(
        poseStack,
        itemDisplayContext,
        packedLight,
        packedOverlay,
        outlineColor,
        this.fadeTintLayers(tintLayers),
        quads,
        this.translucentRenderType(renderType),
        foilType);
  }

  @Override
  public void submitShadow(
      PoseStack poseStack, float shadowStrength, List<EntityRenderState.ShadowPiece> shadowPieces) {
    this.collector.submitShadow(poseStack, shadowStrength, shadowPieces);
  }

  @Override
  public void submitNameTag(
      PoseStack poseStack,
      Vec3 position,
      int backgroundColor,
      Component name,
      boolean seeThrough,
      int packedLight,
      double distance,
      CameraRenderState cameraRenderState) {
    this.collector.submitNameTag(
        poseStack,
        position,
        backgroundColor,
        name,
        seeThrough,
        packedLight,
        distance,
        cameraRenderState);
  }

  @Override
  public void submitText(
      PoseStack poseStack,
      float x,
      float y,
      FormattedCharSequence text,
      boolean dropShadow,
      Font.DisplayMode displayMode,
      int packedLight,
      int color,
      int backgroundColor,
      int outlineColor) {
    this.collector.submitText(
        poseStack,
        x,
        y,
        text,
        dropShadow,
        displayMode,
        packedLight,
        color,
        backgroundColor,
        outlineColor);
  }

  @Override
  public void submitFlame(
      PoseStack poseStack, EntityRenderState renderState, Quaternionf rotation) {
    this.collector.submitFlame(poseStack, renderState, rotation);
  }

  @Override
  public void submitLeash(PoseStack poseStack, EntityRenderState.LeashState leashState) {
    this.collector.submitLeash(poseStack, leashState);
  }

  @Override
  public void submitBlock(
      PoseStack poseStack,
      BlockState blockState,
      int packedLight,
      int packedOverlay,
      int outlineColor) {
    this.collector.submitBlock(poseStack, blockState, packedLight, packedOverlay, outlineColor);
  }

  @Override
  public void submitMovingBlock(PoseStack poseStack, MovingBlockRenderState renderState) {
    this.collector.submitMovingBlock(poseStack, renderState);
  }

  @Override
  public void submitBlockModel(
      PoseStack poseStack,
      RenderType renderType,
      BlockStateModel blockStateModel,
      float red,
      float green,
      float blue,
      int packedLight,
      int packedOverlay,
      int outlineColor) {
    this.collector.submitBlockModel(
        poseStack,
        renderType,
        blockStateModel,
        red,
        green,
        blue,
        packedLight,
        packedOverlay,
        outlineColor);
  }

  @Override
  public void submitCustomGeometry(
      PoseStack poseStack,
      RenderType renderType,
      SubmitNodeCollector.CustomGeometryRenderer customGeometryRenderer) {
    this.collector.submitCustomGeometry(poseStack, renderType, customGeometryRenderer);
  }

  @Override
  public void submitParticleGroup(SubmitNodeCollector.ParticleGroupRenderer particleGroupRenderer) {
    this.collector.submitParticleGroup(particleGroupRenderer);
  }
}
