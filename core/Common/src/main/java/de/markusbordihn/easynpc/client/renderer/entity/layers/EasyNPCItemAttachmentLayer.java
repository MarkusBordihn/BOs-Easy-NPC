/*
 * Copyright 2023 Markus Bordihn
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

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.EasyNPCModelManager;
import de.markusbordihn.easynpc.client.model.EasyNPCModelManagerAccessor;
import de.markusbordihn.easynpc.data.model.ItemAttachmentPoint;
import de.markusbordihn.easynpc.data.model.ModelType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.renderer.ItemInHandRenderer;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.RenderLayerParent;
import net.minecraft.client.renderer.entity.layers.RenderLayer;
import net.minecraft.world.entity.HumanoidArm;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemDisplayContext;
import net.minecraft.world.item.ItemStack;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCItemAttachmentLayer<T extends LivingEntity, M extends EntityModel<T>>
    extends RenderLayer<T, M> {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Set<Item> FAILED_ITEMS = ConcurrentHashMap.newKeySet();

  private final ItemInHandRenderer itemInHandRenderer;
  private boolean renderingDisabled;

  public EasyNPCItemAttachmentLayer(
      RenderLayerParent<T, M> renderer, ItemInHandRenderer itemInHandRenderer) {
    super(renderer);
    this.itemInHandRenderer = itemInHandRenderer;
  }

  private static <M> EasyNPCModelManager getModelManager(M model) {
    if (model instanceof EasyNPCModelManagerAccessor accessor) {
      return accessor.easyNPC$getModelManager();
    }
    return null;
  }

  @Override
  public void render(
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight,
      T entity,
      float limbSwing,
      float limbSwingAmount,
      float partialTick,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {

    if (this.renderingDisabled || !(entity instanceof EasyNPC<?> easyNPC)) {
      return;
    }

    ModelType modelType = easyNPC.getEasyNPCModelData().getModelType();
    if (!modelType.hasItemAttachment()) {
      return;
    }

    ItemStack mainHandItem = entity.getMainHandItem();
    ItemStack offHandItem = entity.getOffhandItem();
    if (mainHandItem.isEmpty() && offHandItem.isEmpty()) {
      return;
    }

    M model = this.getParentModel();
    EasyNPCModelManager modelManager = getModelManager(model);
    if (modelManager == null) {
      return;
    }

    boolean isRightHanded = entity.getMainArm() == HumanoidArm.RIGHT;

    if (!mainHandItem.isEmpty()) {
      ItemAttachmentPoint attachment = getHandAttachment(modelType, true, isRightHanded);
      if (attachment != null && !attachment.isNone()) {
        renderAttachedItemSafely(
            poseStack,
            buffer,
            packedLight,
            modelType,
            entity,
            mainHandItem,
            attachment,
            modelManager,
            isRightHanded
                ? ItemDisplayContext.THIRD_PERSON_RIGHT_HAND
                : ItemDisplayContext.THIRD_PERSON_LEFT_HAND,
            isRightHanded);
      }
    }

    if (!offHandItem.isEmpty()) {
      ItemAttachmentPoint attachment = getHandAttachment(modelType, false, !isRightHanded);
      if (attachment != null && !attachment.isNone()) {
        renderAttachedItemSafely(
            poseStack,
            buffer,
            packedLight,
            modelType,
            entity,
            offHandItem,
            attachment,
            modelManager,
            isRightHanded
                ? ItemDisplayContext.THIRD_PERSON_LEFT_HAND
                : ItemDisplayContext.THIRD_PERSON_RIGHT_HAND,
            !isRightHanded);
      }
    }
  }

  private ItemAttachmentPoint getHandAttachment(
      ModelType modelType, boolean isMainHand, boolean isRightHand) {
    boolean useMainHand =
        modelType == ModelType.HUMANOID || modelType == ModelType.ZOMBIE ? isRightHand : isMainHand;
    return useMainHand ? modelType.getMainHandAttachment() : modelType.getOffHandAttachment();
  }

  private void renderAttachedItemSafely(
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight,
      ModelType modelType,
      T entity,
      ItemStack itemStack,
      ItemAttachmentPoint attachment,
      EasyNPCModelManager modelManager,
      ItemDisplayContext displayContext,
      boolean isRightHand) {
    if (this.renderingDisabled) {
      return;
    }

    Item item = itemStack.getItem();
    if (FAILED_ITEMS.contains(item)) {
      return;
    }

    try {
      renderAttachedItem(
          poseStack,
          buffer,
          packedLight,
          entity,
          itemStack,
          attachment,
          modelManager,
          displayContext,
          isRightHand);
    } catch (LinkageError error) {
      this.renderingDisabled = true;
      log.error(
          "Disabling Easy NPC item attachment rendering for entity {} ({}) with model type {}, model {}, attachment {} after linkage error.",
          entity.getType(),
          entity.getUUID(),
          modelType,
          this.getParentModel().getClass().getName(),
          attachment,
          error);
    } catch (RuntimeException exception) {
      FAILED_ITEMS.add(item);
      log.error(
          "Skipping Easy NPC item attachment for item {} on entity {} ({}) with model type {}, model {}, attachment {} after render error.",
          item,
          entity.getType(),
          entity.getUUID(),
          modelType,
          this.getParentModel().getClass().getName(),
          attachment,
          exception);
    }
  }

  private void renderAttachedItem(
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight,
      T entity,
      ItemStack itemStack,
      ItemAttachmentPoint attachment,
      EasyNPCModelManager modelManager,
      ItemDisplayContext displayContext,
      boolean isRightHand) {

    ModelPart modelPart = modelManager.getModelPart(attachment.attachPart());
    if (modelPart == null || !modelPart.visible) {
      return;
    }

    poseStack.pushPose();
    try {
      modelPart.translateAndRotate(poseStack);

      poseStack.translate(
          attachment.offsetX() / 16.0F, attachment.offsetY() / 16.0F, attachment.offsetZ() / 16.0F);

      if (attachment.rotX() != 0.0F) {
        poseStack.mulPose(Axis.XP.rotation(attachment.rotX()));
      }
      if (attachment.rotY() != 0.0F) {
        poseStack.mulPose(Axis.YP.rotation(attachment.rotY()));
      }
      if (attachment.rotZ() != 0.0F) {
        poseStack.mulPose(Axis.ZP.rotation(attachment.rotZ()));
      }

      float scale = attachment.scale();
      if (scale != 1.0F) {
        poseStack.scale(scale, scale, scale);
      }

      this.itemInHandRenderer.renderItem(
          entity, itemStack, displayContext, !isRightHand, poseStack, buffer, packedLight);
    } finally {
      poseStack.popPose();
    }
  }
}
