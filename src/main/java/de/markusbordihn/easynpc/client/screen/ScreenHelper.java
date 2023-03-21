/**
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

package de.markusbordihn.easynpc.client.screen;

import java.util.Optional;
import java.util.UUID;

import org.joml.Quaternionf;

import com.mojang.blaze3d.platform.Lighting;
import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;

import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRenderDispatcher;
import net.minecraft.core.Rotations;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.Pose;

import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.Profession;
import de.markusbordihn.easynpc.model.ModelPose;
import de.markusbordihn.easynpc.skin.SkinType;

public class ScreenHelper {

  public static void renderEntity(int x, int y, int scale, float yRot, float xRot,
      EasyNPCEntity entity) {
    // Prepare Renderer
    Minecraft minecraft = Minecraft.getInstance();
    float f = (float) Math.atan(yRot / 40.0F);
    float f1 = (float) Math.atan(xRot / 40.0F);
    PoseStack poseStack = RenderSystem.getModelViewStack();
    poseStack.pushPose();
    poseStack.translate(x, y, 1050.0D);
    poseStack.scale(1.0F, 1.0F, -1.0F);
    RenderSystem.applyModelViewMatrix();
    PoseStack poseStack1 = new PoseStack();
    poseStack1.translate(0.0D, 0.0D, 1000.0D);
    poseStack1.scale(scale, scale, scale);
    Quaternionf quaternionf = (new Quaternionf()).rotateZ((float) Math.PI);
    Quaternionf quaternionf1 = (new Quaternionf()).rotateX(f1 * 20.0F * ((float) Math.PI / 180F));
    quaternionf.mul(quaternionf1);
    poseStack1.mulPose(quaternionf);

    // Backup entity information
    Component entityCustomName = entity.getCustomName();
    boolean entityShouldShowName = entity.shouldShowName();
    float entityXRot = entity.getXRot();
    float entityYBodyRot = entity.yBodyRot;
    float entityYHeadRot = entity.yHeadRot;
    float entityYHeadRotO = entity.yHeadRotO;
    float entityYRot = entity.getYRot();

    // Adjust entity information for rendering
    entity.yBodyRot = 180.0F + f * 20.0F;
    entity.setYRot(180.0F + f * 40.0F);
    entity.setXRot(-f1 * 20.0F);
    entity.yHeadRot = entity.getYRot();

    // Hide gui elements
    boolean minecraftHideGui = false;
    if (minecraft != null) {
      minecraftHideGui = minecraft.options.hideGui;
      minecraft.options.hideGui = true;
    } else {
      entity.setCustomName(null);
      entity.setCustomNameVisible(false);
    }

    // Render Entity
    Lighting.setupForEntityInInventory();
    EntityRenderDispatcher entityRenderDispatcher =
        Minecraft.getInstance().getEntityRenderDispatcher();
    quaternionf1.conjugate();
    entityRenderDispatcher.overrideCameraOrientation(quaternionf1);
    entityRenderDispatcher.setRenderShadow(false);
    MultiBufferSource.BufferSource multiBuffer =
        Minecraft.getInstance().renderBuffers().bufferSource();
    entityRenderDispatcher.render(entity, 0.0D, 0.0D, 0.0D, 0.0F, 1.0F, poseStack1, multiBuffer,
        15728880);
    multiBuffer.endBatch();
    entityRenderDispatcher.setRenderShadow(true);

    // Show gui elements
    if (minecraft != null) {
      minecraft.options.hideGui = minecraftHideGui;
    }

    // Restore entity information
    entity.yBodyRot = entityYBodyRot;
    entity.setYRot(entityYRot);
    entity.setXRot(entityXRot);
    entity.yHeadRot = entityYHeadRot;
    entity.yHeadRotO = entityYHeadRotO;

    // Restore gui elements or custom name
    if (minecraft != null) {
      minecraft.options.hideGui = minecraftHideGui;
    } else {
      entity.setCustomName(entityCustomName);
      entity.setCustomNameVisible(entityShouldShowName);
    }

    poseStack.popPose();
    RenderSystem.applyModelViewMatrix();
    Lighting.setupFor3DItems();
  }

  public static void renderScaledEntityAvatar(int x, int y, int scale, float yRot, float xRot,
      EasyNPCEntity entity) {
    // Backup entity information
    float entityScaleX = entity.getScaleX();
    float entityScaleY = entity.getScaleY();
    float entityScaleZ = entity.getScaleZ();
    Rotations entityModelRootRotation = entity.getModelRootRotation();
    boolean entityInvisible = entity.isInvisible();

    // Adjust entity information for rendering
    entity.setScaleX(entity.getDefaultScaleX());
    entity.setScaleY(entity.getDefaultScaleY());
    entity.setScaleZ(entity.getDefaultScaleZ());
    entity.setModelRootRotation(new Rotations(0.0F, 0.0F, 0.0F));
    entity.setInvisible(false);

    // Render Entity
    renderEntity(x, y, scale, yRot, xRot, entity);

    // Restore entity information
    entity.setScaleX(entityScaleX);
    entity.setScaleY(entityScaleY);
    entity.setScaleZ(entityScaleZ);
    entity.setModelRootRotation(entityModelRootRotation);
    entity.setInvisible(entityInvisible);
  }

  public static void renderCustomPoseEntityAvatar(int x, int y, int scale, float yRot, float xRot,
      EasyNPCEntity entity) {
    // Backup entity information
    ModelPose entityModelPose = entity.getModelPose();
    Pose entityPose = entity.getPose();

    // Adjust entity information for rendering
    entity.setModelPose(ModelPose.CUSTOM);
    entity.setPose(Pose.STANDING);

    // Render Entity
    renderScaledEntityAvatar(x, y, scale, yRot, xRot, entity);

    // Restore entity information
    entity.setModelPose(entityModelPose);
    entity.setPose(entityPose);
  }


  public static void renderEntityAvatar(int x, int y, float yRot, float xRot,
      EasyNPCEntity entity) {
    renderEntity(x, y, entity.getEntityGuiScaling(), yRot, xRot, entity);
  }

  public static void renderEntityAvatar(int x, int y, int scale, float yRot, float xRot,
      EasyNPCEntity entity) {
    renderEntity(x, y, scale, yRot, xRot, entity);
  }

  public static void renderEntityAvatarForScaling(int x, int y, int scale, float yRot, float xRot,
      EasyNPCEntity entity) {
    // Backup entity information
    Rotations entityModelRootRotation = entity.getModelRootRotation();
    boolean entityInvisible = entity.isInvisible();

    // Adjust entity information for rendering
    entity.setModelRootRotation(new Rotations(0.0F, 0.0F, 0.0F));
    entity.setInvisible(false);

    // Render Entity
    renderEntity(x, y, scale, yRot, xRot, entity);

    // Restore entity information
    entity.setModelRootRotation(entityModelRootRotation);
    entity.setInvisible(entityInvisible);
  }

  public static void renderEntityDialog(int x, int y, float yRot, float xRot,
      EasyNPCEntity entity) {
    renderScaledEntityAvatar(x, y, entity.getEntityDialogScaling(), yRot, xRot, entity);
  }

  public static void renderEntityPlayerSkin(int x, int y, float yRot, float xRot,
      EasyNPCEntity entity, UUID userUUID, SkinType skinType) {

    // Backup entity information
    SkinType entitySkinType = entity.getSkinType();
    Optional<UUID> entitySkinUUID = entity.getSkinUUID();
    boolean entityPreview = entity.isPreview();

    // Adjust entity information for rendering
    entity.setSkinType(skinType);
    entity.setSkinUUID(userUUID);
    entity.setPreview(true);

    // Render Entity
    renderScaledEntityAvatar(x, y, entity.getEntityGuiScaling(), yRot, xRot, entity);

    // Restore entity information
    entity.setSkinType(entitySkinType);
    entity.setSkinUUID(entitySkinUUID);
    entity.setPreview(entityPreview);
  }

  public static void renderEntityDefaultSkin(int x, int y, float yRot, float xRot,
      EasyNPCEntity entity, Enum<?> variant, Profession profession) {

    // Backup entity information
    SkinType entitySkinType = entity.getSkinType();
    Enum<?> entityVariant = entity.getVariant();
    Profession entityProfession = entity.getProfession();

    // Adjust entity information for rendering
    entity.setSkinType(SkinType.DEFAULT);
    entity.setVariant(variant);
    entity.setProfession(profession);

    // Render Entity
    renderScaledEntityAvatar(x, y, entity.getEntityGuiScaling(), yRot, xRot, entity);

    // Restore entity information
    entity.setSkinType(entitySkinType);
    entity.setVariant(entityVariant);
    entity.setProfession(entityProfession);
  }

}
