/*
 * Copyright 2022 Markus Bordihn
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

package de.markusbordihn.easynpc.client.renderer.entity.layers;

import com.google.common.collect.Maps;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import java.util.Map;
import javax.annotation.Nullable;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.Sheets;
import net.minecraft.client.renderer.entity.ItemRenderer;
import net.minecraft.client.renderer.entity.RenderLayerParent;
import net.minecraft.client.renderer.entity.layers.HumanoidArmorLayer;
import net.minecraft.client.renderer.texture.OverlayTexture;
import net.minecraft.client.renderer.texture.TextureAtlas;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.resources.model.ModelManager;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.ArmorItem;
import net.minecraft.world.item.ArmorMaterial;
import net.minecraft.world.item.DyeableArmorItem;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.armortrim.ArmorTrim;

public class CustomHumanoidArmorLayer<
        T extends LivingEntity, M extends HumanoidModel<T>, A extends HumanoidModel<T>>
    extends HumanoidArmorLayer<T, M, A> {

  private static final Map<String, ResourceLocation> ARMOR_LOCATION_CACHE = Maps.newHashMap();

  private final A innerModel;
  private final A outerModel;
  private final TextureAtlas armorTrimAtlas;

  public CustomHumanoidArmorLayer(
      RenderLayerParent<T, M> renderer, A innerModel, A outerModel, ModelManager modelManager) {
    super(renderer, innerModel, outerModel, modelManager);
    this.innerModel = innerModel;
    this.outerModel = outerModel;
    this.armorTrimAtlas = modelManager.getAtlas(Sheets.ARMOR_TRIMS_SHEET);
  }

  @Override
  public void render(
      PoseStack poseStack,
      MultiBufferSource multiBufferSource,
      int $$2,
      T livingEntity,
      float unused1,
      float unused2,
      float unused3,
      float unused4,
      float unused5,
      float unused6) {
    ModelData<T> modelData =
        livingEntity instanceof EasyNPC easyNPC ? easyNPC.getEasyNPCModelData() : null;
    if (modelData == null || modelData.isModelChestplateVisible()) {
      this.renderArmorPiece(
          poseStack,
          multiBufferSource,
          livingEntity,
          EquipmentSlot.CHEST,
          $$2,
          this.getArmorModel(EquipmentSlot.CHEST));
    }
    if (modelData == null || modelData.isModelLeggingsVisible()) {
      this.renderArmorPiece(
          poseStack,
          multiBufferSource,
          livingEntity,
          EquipmentSlot.LEGS,
          $$2,
          this.getArmorModel(EquipmentSlot.LEGS));
    }
    if (modelData == null || modelData.isModelBootsVisible()) {
      this.renderArmorPiece(
          poseStack,
          multiBufferSource,
          livingEntity,
          EquipmentSlot.FEET,
          $$2,
          this.getArmorModel(EquipmentSlot.FEET));
    }
    if (modelData == null || modelData.isModelHelmetVisible()) {
      this.renderArmorPiece(
          poseStack,
          multiBufferSource,
          livingEntity,
          EquipmentSlot.HEAD,
          $$2,
          this.getArmorModel(EquipmentSlot.HEAD));
    }
  }

  private void renderArmorPiece(
      PoseStack poseStack,
      MultiBufferSource multiBufferSource,
      T livingEntity,
      EquipmentSlot equipmentSlot,
      int light,
      A humanoidModel) {
    ItemStack itemStack = livingEntity.getItemBySlot(equipmentSlot);
    if (itemStack.getItem() instanceof ArmorItem armorItem
        && (armorItem.getEquipmentSlot() == equipmentSlot)) {
      this.getParentModel().copyPropertiesTo(humanoidModel);
      this.setPartVisibility(humanoidModel, equipmentSlot);
      boolean usesInnerModel = this.usesInnerModel(equipmentSlot);
      boolean hasFoil = itemStack.hasFoil();
      if (armorItem instanceof DyeableArmorItem dyeableArmorItem) {
        int dyeableArmorItemColor = dyeableArmorItem.getColor(itemStack);
        float red = (dyeableArmorItemColor >> 16 & 255) / 255.0F;
        float green = (dyeableArmorItemColor >> 8 & 255) / 255.0F;
        float blue = (dyeableArmorItemColor & 255) / 255.0F;
        this.renderModel(
            poseStack,
            multiBufferSource,
            light,
            armorItem,
            hasFoil,
            humanoidModel,
            usesInnerModel,
            red,
            green,
            blue,
            (String) null);
        this.renderModel(
            poseStack,
            multiBufferSource,
            light,
            armorItem,
            hasFoil,
            humanoidModel,
            usesInnerModel,
            1.0F,
            1.0F,
            1.0F,
            "overlay");
      } else {
        this.renderModel(
            poseStack,
            multiBufferSource,
            light,
            armorItem,
            hasFoil,
            humanoidModel,
            usesInnerModel,
            1.0F,
            1.0F,
            1.0F,
            (String) null);
      }

      ArmorTrim.getTrim(livingEntity.level().registryAccess(), itemStack)
          .ifPresent(
              armorTrim ->
                  this.renderTrim(
                      armorItem.getMaterial(),
                      poseStack,
                      multiBufferSource,
                      light,
                      armorTrim,
                      hasFoil,
                      humanoidModel,
                      usesInnerModel,
                      1.0F,
                      1.0F,
                      1.0F));
      if (itemStack.hasFoil()) {
        this.renderGlint(poseStack, multiBufferSource, light, humanoidModel);
      }
    }
  }

  private void renderModel(
      PoseStack poseStack,
      MultiBufferSource multiBufferSource,
      int light,
      ArmorItem armorItem,
      boolean hasFoil,
      A humanoidModel,
      boolean layer,
      float red,
      float green,
      float blue,
      @Nullable String overlayName) {
    VertexConsumer vertexConsumer =
        ItemRenderer.getArmorFoilBuffer(
            multiBufferSource,
            RenderType.armorCutoutNoCull(this.getArmorLocation(armorItem, layer, overlayName)),
            false,
            hasFoil);
    humanoidModel.renderToBuffer(
        poseStack, vertexConsumer, light, OverlayTexture.NO_OVERLAY, red, green, blue, 1.0F);
  }

  private void renderTrim(
      ArmorMaterial armorMaterial,
      PoseStack poseStack,
      MultiBufferSource multiBufferSource,
      int light,
      ArmorTrim armorTrim,
      boolean hasFoil,
      A humanoidModel,
      boolean layer,
      float red,
      float green,
      float blue) {
    TextureAtlasSprite textureAtlasSprite =
        this.armorTrimAtlas.getSprite(
            layer ? armorTrim.innerTexture(armorMaterial) : armorTrim.outerTexture(armorMaterial));
    VertexConsumer vertexConsumer =
        textureAtlasSprite.wrap(
            ItemRenderer.getFoilBufferDirect(
                multiBufferSource, Sheets.armorTrimsSheet(), true, hasFoil));
    humanoidModel.renderToBuffer(
        poseStack, vertexConsumer, light, OverlayTexture.NO_OVERLAY, red, green, blue, 1.0F);
  }

  private void renderGlint(
      PoseStack poseStack, MultiBufferSource multiBufferSource, int light, A humanoidModel) {
    humanoidModel.renderToBuffer(
        poseStack,
        multiBufferSource.getBuffer(RenderType.armorEntityGlint()),
        light,
        OverlayTexture.NO_OVERLAY,
        1.0F,
        1.0F,
        1.0F,
        1.0F);
  }

  private A getArmorModel(EquipmentSlot equipmentSlot) {
    return this.usesInnerModel(equipmentSlot) ? this.innerModel : this.outerModel;
  }

  private boolean usesInnerModel(EquipmentSlot equipmentSlot) {
    return equipmentSlot == EquipmentSlot.LEGS;
  }

  private ResourceLocation getArmorLocation(
      ArmorItem armorItem, boolean layer, @Nullable String overlayName) {
    String texture =
        "textures/models/armor/"
            + armorItem.getMaterial().getName()
            + "_layer_"
            + (layer ? 2 : 1)
            + (overlayName == null ? "" : "_" + overlayName)
            + ".png";
    return ARMOR_LOCATION_CACHE.computeIfAbsent(texture, ResourceLocation::new);
  }
}
