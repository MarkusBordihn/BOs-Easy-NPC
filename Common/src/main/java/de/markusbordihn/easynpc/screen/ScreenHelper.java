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

package de.markusbordihn.easynpc.screen;

import com.mojang.blaze3d.platform.Lighting;
import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Quaternion;
import com.mojang.math.Vector3f;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.profession.Profession;
import de.markusbordihn.easynpc.data.render.RenderDataSet;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.entity.easynpc.data.GuiData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.entity.easynpc.data.ProfessionData;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderData;
import de.markusbordihn.easynpc.entity.easynpc.data.ScaleData;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantData;
import java.util.UUID;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRenderDispatcher;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.scores.PlayerTeam;
import net.minecraft.world.scores.Team;

public class ScreenHelper {

  protected ScreenHelper() {}

  public static void renderEntity(
      int x, int y, int scale, float yRot, float xRot, LivingEntity livingEntity) {
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
    Quaternion quaternion = Vector3f.ZP.rotationDegrees(180.0F);
    Quaternion quaternion1 = Vector3f.XP.rotationDegrees(f1 * 20.0F);
    quaternion.mul(quaternion1);
    poseStack1.mulPose(quaternion);

    // Backup entity information
    Component entityCustomName = livingEntity.getCustomName();
    boolean entityShouldShowName = livingEntity.shouldShowName();
    float entityXRot = livingEntity.getXRot();
    float entityYBodyRot = livingEntity.yBodyRot;
    float entityYHeadRot = livingEntity.yHeadRot;
    float entityYHeadRotO = livingEntity.yHeadRotO;
    float entityYRot = livingEntity.getYRot();
    boolean entityInvisible = livingEntity.isInvisible();
    Team entityTeam = livingEntity.getTeam();

    // Adjust entity information for rendering
    livingEntity.setInvisible(false);
    livingEntity.yBodyRot = 180.0F + f * 20.0F;
    livingEntity.setYRot(180.0F + f * 40.0F);
    livingEntity.setXRot(-f1 * 20.0F);
    livingEntity.yHeadRot = livingEntity.getYRot();
    if (entityTeam instanceof PlayerTeam playerTeam) {
      livingEntity
          .level
          .getScoreboard()
          .removePlayerFromTeam(livingEntity.getScoreboardName(), playerTeam);
    }

    // Hide gui elements or remove custom name
    boolean minecraftHideGui = false;
    if (minecraft != null) {
      minecraftHideGui = minecraft.options.hideGui;
      minecraft.options.hideGui = true;
    } else {
      livingEntity.setCustomName(null);
      livingEntity.setCustomNameVisible(false);
    }

    // Render Entity
    Lighting.setupForEntityInInventory();
    EntityRenderDispatcher entityRenderDispatcher =
        Minecraft.getInstance().getEntityRenderDispatcher();
    quaternion1.conj();
    entityRenderDispatcher.overrideCameraOrientation(quaternion1);
    entityRenderDispatcher.setRenderShadow(false);
    MultiBufferSource.BufferSource multiBuffer =
        Minecraft.getInstance().renderBuffers().bufferSource();
    entityRenderDispatcher.render(
        livingEntity, 0.0D, 0.0D, 0.0D, 0.0F, 1.0F, poseStack1, multiBuffer, 15728880);
    multiBuffer.endBatch();
    entityRenderDispatcher.setRenderShadow(true);

    // Restore entity information
    livingEntity.setInvisible(entityInvisible);
    livingEntity.yBodyRot = entityYBodyRot;
    livingEntity.setYRot(entityYRot);
    livingEntity.setXRot(entityXRot);
    livingEntity.yHeadRot = entityYHeadRot;
    livingEntity.yHeadRotO = entityYHeadRotO;
    if (entityTeam instanceof PlayerTeam playerTeam) {
      livingEntity
          .level
          .getScoreboard()
          .addPlayerToTeam(livingEntity.getScoreboardName(), playerTeam);
    }

    // Restore gui elements or custom name
    if (minecraft != null) {
      minecraft.options.hideGui = minecraftHideGui;
    } else {
      livingEntity.setCustomName(entityCustomName);
      livingEntity.setCustomNameVisible(entityShouldShowName);
    }

    poseStack.popPose();
    RenderSystem.applyModelViewMatrix();
    Lighting.setupFor3DItems();
  }

  public static void renderScaledEntityAvatar(
      int x, int y, int scale, float yRot, float xRot, EasyNPC<?> easyNPC) {
    ScaleData<?> scaleData = easyNPC.getEasyNPCScaleData();
    ModelData<?> modelData = easyNPC.getEasyNPCModelData();
    if (scaleData != null && modelData != null) {
      renderScaledEntityAvatar(x, y, scale, yRot, xRot, easyNPC, scaleData, modelData);
    } else {
      renderEntity(x, y, scale, yRot, xRot, easyNPC.getLivingEntity());
    }
  }

  public static void renderScaledEntityAvatar(
      int x, int y, float yRot, float xRot, EasyNPC<?> easyNPC) {
    GuiData<?> guiData = easyNPC.getEasyNPCGuiData();
    renderScaledEntityAvatar(x, y, guiData.getEntityGuiScaling(), yRot, xRot, easyNPC);
  }

  public static void renderScaledEntityAvatar(
      int x,
      int y,
      int scale,
      float yRot,
      float xRot,
      EasyNPC<?> easyNPC,
      ScaleData<?> scaleData,
      ModelData<?> modelData) {

    // Backup entity information
    float entityScaleX = scaleData != null ? scaleData.getScaleX() : 0.4F;
    float entityScaleY = scaleData != null ? scaleData.getScaleY() : 0.4F;
    float entityScaleZ = scaleData != null ? scaleData.getScaleZ() : 0.4F;
    CustomRotation entityModelRootRotation =
        modelData != null ? modelData.getModelRootRotation() : null;

    // Adjust entity information for rendering
    if (scaleData != null) {
      scaleData.setScaleX(scaleData.getDefaultScaleX());
      scaleData.setScaleY(scaleData.getDefaultScaleY());
      scaleData.setScaleZ(scaleData.getDefaultScaleZ());
    }
    if (modelData != null) {
      modelData.setModelRootRotation(new CustomRotation(0.0F, 0.0F, 0.0F));
    }

    // Render Entity
    renderEntity(x, y, scale, yRot, xRot, easyNPC.getLivingEntity());

    // Restore entity information
    if (scaleData != null) {
      scaleData.setScaleX(entityScaleX);
      scaleData.setScaleY(entityScaleY);
      scaleData.setScaleZ(entityScaleZ);
    }
    if (modelData != null && entityModelRootRotation != null) {
      modelData.setModelRootRotation(entityModelRootRotation);
    }
  }

  public static void renderCustomPoseEntityAvatar(
      int x, int y, int scale, float yRot, float xRot, EasyNPC<?> easyNPC) {
    ModelData<?> modelData = easyNPC.getEasyNPCModelData();
    Entity entity = easyNPC.getEntity();

    // Backup entity information
    ModelPose entityModelPose = modelData.getModelPose();
    Pose entityPose = easyNPC.getEntity().getPose();

    // Adjust entity information for rendering
    modelData.setModelPose(ModelPose.CUSTOM);
    entity.setPose(Pose.STANDING);

    // Render Entity
    renderScaledEntityAvatar(
        x, y, scale, yRot, xRot, easyNPC, easyNPC.getEasyNPCScaleData(), modelData);

    // Restore entity information
    modelData.setModelPose(entityModelPose);
    entity.setPose(entityPose);
  }

  public static void renderEntityAvatarForScaling(
      int x, int y, int scale, float yRot, float xRot, EasyNPC<?> easyNPC) {
    ModelData<?> modelData = easyNPC.getEasyNPCModelData();
    Entity entity = easyNPC.getEntity();

    // Backup entity information
    CustomRotation entityModelRootRotation = modelData.getModelRootRotation();
    boolean entityInvisible = entity.isInvisible();

    // Adjust entity information for rendering
    modelData.setModelRootRotation(new CustomRotation(0.0F, 0.0F, 0.0F));
    entity.setInvisible(false);

    // Render Entity
    renderEntity(x, y, scale, yRot, xRot, easyNPC.getLivingEntity());

    // Restore entity information
    modelData.setModelRootRotation(entityModelRootRotation);
    entity.setInvisible(entityInvisible);
  }

  public static void renderEntityDialog(int x, int y, float yRot, float xRot, EasyNPC<?> easyNPC) {
    DialogData<?> dialogData = easyNPC.getEasyNPCDialogData();
    renderEntity(x, y, dialogData.getEntityDialogScaling(), yRot, xRot, easyNPC.getLivingEntity());
  }

  public static void renderEntityCustomModel(
      int x,
      int y,
      int scale,
      float yRot,
      float xRot,
      EasyNPC<?> easyNPC,
      EntityType<? extends Entity> entityType) {
    GuiData<?> guiData = easyNPC.getEasyNPCGuiData();
    RenderData<?> renderData = easyNPC.getEasyNPCRenderData();
    RenderDataSet renderDataSet = renderData.getRenderDataSet();

    // Backup renderer information
    RenderType renderType = renderDataSet.getRenderType();
    EntityType<?> renderEntityType = renderDataSet.getRenderEntityType();

    // Adjust entity information for rendering
    renderDataSet.setRenderType(RenderType.CUSTOM);
    renderDataSet.setRenderEntityType(entityType);

    // Render Entity
    renderScaledEntityAvatar(
        x + guiData.getEntityGuiLeft(),
        y + guiData.getEntityGuiTop(),
        scale,
        yRot,
        xRot,
        easyNPC,
        easyNPC.getEasyNPCScaleData(),
        easyNPC.getEasyNPCModelData());

    // Restore renderer information
    renderDataSet.setRenderType(renderType);
    renderDataSet.setRenderEntityType(renderEntityType);
  }

  public static void renderEntityPlayerSkin(
      int x, int y, float yRot, float xRot, EasyNPC<?> easyNPC, UUID userUUID, SkinType skinType) {
    SkinData<?> skinData = easyNPC.getEasyNPCSkinData();
    GuiData<?> guiData = easyNPC.getEasyNPCGuiData();

    // Backup entity information
    SkinType entitySkinType = skinData.getSkinType();
    UUID entitySkinUUID = skinData.getSkinUUID();

    // Adjust entity information for rendering
    skinData.setSkinType(skinType);
    skinData.setSkinUUID(userUUID);

    // Render Entity
    renderScaledEntityAvatar(
        x + guiData.getEntityGuiLeft(),
        y + guiData.getEntityGuiTop(),
        skinData.getEntitySkinScaling(),
        yRot,
        xRot,
        easyNPC,
        easyNPC.getEasyNPCScaleData(),
        easyNPC.getEasyNPCModelData());

    // Restore entity information
    skinData.setSkinType(entitySkinType);
    skinData.setSkinUUID(entitySkinUUID);
  }

  public static void renderEntityDefaultSkin(
      int x,
      int y,
      float yRot,
      float xRot,
      EasyNPC<?> easyNPC,
      Enum<?> variant,
      Profession profession) {
    SkinData<?> skinData = easyNPC.getEasyNPCSkinData();
    VariantData<?> variantData = easyNPC.getEasyNPCVariantData();
    ProfessionData<?> professionData = easyNPC.getEasyNPCProfessionData();
    GuiData<?> guiData = easyNPC.getEasyNPCGuiData();

    // Backup entity information
    SkinType entitySkinType = skinData.getSkinType();
    Enum<?> entityVariant = variantData.getVariant();
    Profession entityProfession = professionData.getProfession();

    // Adjust entity information for rendering
    skinData.setSkinType(SkinType.DEFAULT);
    variantData.setVariant(variant);
    professionData.setProfession(profession);

    // Render Entity
    renderEntity(
        x + guiData.getEntityGuiLeft(),
        y + guiData.getEntityGuiTop(),
        skinData.getEntitySkinScaling(),
        yRot,
        xRot,
        easyNPC.getLivingEntity());

    // Restore entity information
    skinData.setSkinType(entitySkinType);
    variantData.setVariant(entityVariant);
    professionData.setProfession(entityProfession);
  }
}
