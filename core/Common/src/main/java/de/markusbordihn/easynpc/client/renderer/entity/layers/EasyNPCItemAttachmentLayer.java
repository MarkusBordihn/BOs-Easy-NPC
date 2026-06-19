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
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.data.model.ItemAttachmentPoint;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.client.Minecraft;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.renderer.SubmitNodeCollector;
import net.minecraft.client.renderer.entity.RenderLayerParent;
import net.minecraft.client.renderer.entity.layers.RenderLayer;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import net.minecraft.client.renderer.item.ItemStackRenderState;
import net.minecraft.client.renderer.texture.OverlayTexture;
import net.minecraft.world.entity.HumanoidArm;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.monster.illager.AbstractIllager;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemDisplayContext;
import net.minecraft.world.item.ItemStack;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCItemAttachmentLayer<
        S extends LivingEntityRenderState, M extends EntityModel<? super S>>
    extends RenderLayer<S, M> {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Set<Item> FAILED_ITEMS = ConcurrentHashMap.newKeySet();

  private static final ItemAttachmentPoint ILLAGER_CROSSED_ARMS_ATTACHMENT =
      new ItemAttachmentPoint(
          ModelPartType.BODY, 0.0F, 7.0F, -7.0F, 0.0F, 0.0F, (float) Math.PI, 0.6F);

  private final ItemStackRenderState mainHandRenderState = new ItemStackRenderState();
  private final ItemStackRenderState offHandRenderState = new ItemStackRenderState();
  private boolean renderingDisabled;

  public EasyNPCItemAttachmentLayer(RenderLayerParent<S, M> renderer) {
    super(renderer);
  }

  private static <M> EasyNPCModelManager getModelManager(M model) {
    if (model instanceof EasyNPCModelManagerAccessor accessor) {
      return accessor.easyNPC$getModelManager();
    }
    return null;
  }

  @Override
  public void submit(
      PoseStack poseStack,
      SubmitNodeCollector submitNodeCollector,
      int packedLight,
      S renderState,
      float limbSwing,
      float limbSwingAmount) {

    if (this.renderingDisabled) {
      return;
    }

    if (!(renderState instanceof EasyNPCRenderStateExtension renderStateExtension)) {
      return;
    }

    UUID entityUUID = renderStateExtension.getEasyNpcUUID();
    if (entityUUID == null) {
      return;
    }

    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(entityUUID);
    if (easyNPC == null) {
      return;
    }

    LivingEntity entity = (LivingEntity) easyNPC.getEntity();
    if (entity == null) {
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

    var itemModelResolver = Minecraft.getInstance().getItemModelResolver();

    if (!mainHandItem.isEmpty()) {
      ItemAttachmentPoint attachment = getHandAttachment(modelType, true, isRightHanded);
      if (attachment != null && !attachment.isNone()) {
        Item item = mainHandItem.getItem();
        if (!FAILED_ITEMS.contains(item)) {
          ItemDisplayContext displayContext =
              isRightHanded
                  ? ItemDisplayContext.THIRD_PERSON_RIGHT_HAND
                  : ItemDisplayContext.THIRD_PERSON_LEFT_HAND;
          mainHandRenderState.clear();
          itemModelResolver.updateForLiving(
              mainHandRenderState, mainHandItem, displayContext, entity);
          renderAttachedItemSafely(
              poseStack,
              submitNodeCollector,
              packedLight,
              modelType,
              entity,
              item,
              mainHandRenderState,
              attachment,
              modelManager,
              renderState);
        }
      }
    }

    if (!offHandItem.isEmpty()) {
      ItemAttachmentPoint attachment = getHandAttachment(modelType, false, !isRightHanded);
      if (attachment != null && !attachment.isNone()) {
        Item item = offHandItem.getItem();
        if (!FAILED_ITEMS.contains(item)) {
          ItemDisplayContext displayContext =
              isRightHanded
                  ? ItemDisplayContext.THIRD_PERSON_LEFT_HAND
                  : ItemDisplayContext.THIRD_PERSON_RIGHT_HAND;
          offHandRenderState.clear();
          itemModelResolver.updateForLiving(
              offHandRenderState, offHandItem, displayContext, entity);
          renderAttachedItemSafely(
              poseStack,
              submitNodeCollector,
              packedLight,
              modelType,
              entity,
              item,
              offHandRenderState,
              attachment,
              modelManager,
              renderState);
        }
      }
    }
  }

  private ItemAttachmentPoint getHandAttachment(
      ModelType modelType, boolean isMainHand, boolean isRightHand) {
    boolean useMainHand = modelType == ModelType.HUMANOID ? isRightHand : isMainHand;
    return useMainHand ? modelType.getMainHandAttachment() : modelType.getOffHandAttachment();
  }

  private void renderAttachedItemSafely(
      PoseStack poseStack,
      SubmitNodeCollector submitNodeCollector,
      int packedLight,
      ModelType modelType,
      LivingEntity entity,
      Item item,
      ItemStackRenderState itemRenderState,
      ItemAttachmentPoint attachment,
      EasyNPCModelManager modelManager,
      S renderState) {
    try {
      renderAttachedItem(
          poseStack,
          submitNodeCollector,
          packedLight,
          itemRenderState,
          attachment,
          modelManager,
          entity,
          renderState);
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
      SubmitNodeCollector submitNodeCollector,
      int packedLight,
      ItemStackRenderState itemRenderState,
      ItemAttachmentPoint attachment,
      EasyNPCModelManager modelManager,
      LivingEntity entity,
      S renderState) {

    if (itemRenderState.isEmpty()) {
      return;
    }

    ModelPartType attachPart = attachment.attachPart();
    boolean isHandAttachment =
        attachPart == ModelPartType.RIGHT_ARM || attachPart == ModelPartType.LEFT_ARM;

    if (isHandAttachment
        && entity instanceof AbstractIllager illager
        && illager.getArmPose() == AbstractIllager.IllagerArmPose.CROSSED) {
      attachment = ILLAGER_CROSSED_ARMS_ATTACHMENT;
      attachPart = attachment.attachPart();
    }

    ModelPart modelPart = modelManager.getModelPart(attachPart);
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

      itemRenderState.submit(
          poseStack,
          submitNodeCollector,
          packedLight,
          OverlayTexture.NO_OVERLAY,
          renderState.outlineColor);
    } finally {
      poseStack.popPose();
    }
  }
}
