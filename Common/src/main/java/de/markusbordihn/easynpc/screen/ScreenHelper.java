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
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.renderer.entity.EntityRenderDispatcher;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.scores.PlayerTeam;
import net.minecraft.world.scores.Team;
import org.joml.Quaternionf;

public class ScreenHelper {

  protected ScreenHelper() {}

  public static void renderEntity(
      GuiGraphics guiGraphics,
      int x,
      int y,
      int scale,
      float yRot,
      float xRot,
      LivingEntity livingEntity) {
    // Prepare Renderer
    Minecraft minecraft = Minecraft.getInstance();
    float rotationY = (float) Math.atan(yRot / 40.0F);
    float rotationX = (float) Math.atan(xRot / 40.0F);
    Quaternionf quaternionfZ = (new Quaternionf()).rotateZ(3.1415927F);
    Quaternionf quaternionfX = (new Quaternionf()).rotateX(rotationX * 20.0F * 0.017453292F);
    quaternionfZ.mul(quaternionfX);

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
    livingEntity.yBodyRot = 180.0F + rotationY * 20.0F;
    livingEntity.setYRot(180.0F + rotationY * 40.0F);
    livingEntity.setXRot(-rotationX * 20.0F);
    livingEntity.yHeadRot = livingEntity.getYRot();
    livingEntity.yHeadRotO = livingEntity.getYRot();
    if (entityTeam instanceof PlayerTeam playerTeam) {
      livingEntity
          .level()
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
    guiGraphics.pose().pushPose();
    guiGraphics.pose().translate(x, y, 1050.0D);
    guiGraphics.pose().scale(scale, scale, -scale);
    guiGraphics.pose().mulPose(quaternionfZ);
    Lighting.setupForEntityInInventory();
    EntityRenderDispatcher entityRenderDispatcher =
        Minecraft.getInstance().getEntityRenderDispatcher();
    quaternionfX.conjugate();
    entityRenderDispatcher.overrideCameraOrientation(quaternionfX);
    entityRenderDispatcher.setRenderShadow(false);
    entityRenderDispatcher.render(
        livingEntity,
        0.0D,
        0.0D,
        0.0D,
        0.0F,
        1.0F,
        guiGraphics.pose(),
        guiGraphics.bufferSource(),
        15728880);
    guiGraphics.flush();
    entityRenderDispatcher.setRenderShadow(true);
    guiGraphics.pose().popPose();
    Lighting.setupFor3DItems();

    // Restore entity information
    livingEntity.setInvisible(entityInvisible);
    livingEntity.yBodyRot = entityYBodyRot;
    livingEntity.setYRot(entityYRot);
    livingEntity.setXRot(entityXRot);
    livingEntity.yHeadRot = entityYHeadRot;
    livingEntity.yHeadRotO = entityYHeadRotO;
    if (entityTeam instanceof PlayerTeam playerTeam) {
      livingEntity
          .level()
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
  }

  public static void renderScaledEntityAvatar(
      GuiGraphics guiGraphics,
      int x,
      int y,
      int scale,
      float yRot,
      float xRot,
      EasyNPC<?> easyNPC) {
    ScaleData<?> scaleData = easyNPC.getEasyNPCScaleData();
    ModelData<?> modelData = easyNPC.getEasyNPCModelData();
    if (scaleData != null && modelData != null) {
      renderScaledEntityAvatar(guiGraphics, x, y, scale, yRot, xRot, easyNPC, scaleData, modelData);
    } else {
      renderEntity(guiGraphics, x, y, scale, yRot, xRot, easyNPC.getLivingEntity());
    }
  }

  public static void renderScaledEntityAvatar(
      GuiGraphics guiGraphics, int x, int y, float yRot, float xRot, EasyNPC<?> easyNPC) {
    GuiData<?> guiData = easyNPC.getEasyNPCGuiData();
    renderScaledEntityAvatar(guiGraphics, x, y, guiData.getEntityGuiScaling(), yRot, xRot, easyNPC);
  }

  public static void renderScaledEntityAvatar(
      GuiGraphics guiGraphics,
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
    renderEntity(guiGraphics, x, y, scale, yRot, xRot, easyNPC.getLivingEntity());

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
      GuiGraphics guiGraphics,
      int x,
      int y,
      int scale,
      float yRot,
      float xRot,
      EasyNPC<?> easyNPC) {
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
        guiGraphics, x, y, scale, yRot, xRot, easyNPC, easyNPC.getEasyNPCScaleData(), modelData);

    // Restore entity information
    modelData.setModelPose(entityModelPose);
    entity.setPose(entityPose);
  }

  public static void renderEntityAvatarForScaling(
      GuiGraphics guiGraphics,
      int x,
      int y,
      int scale,
      float yRot,
      float xRot,
      EasyNPC<?> easyNPC) {
    ModelData<?> modelData = easyNPC.getEasyNPCModelData();
    Entity entity = easyNPC.getEntity();

    // Backup entity information
    CustomRotation entityModelRootRotation = modelData.getModelRootRotation();
    boolean entityInvisible = entity.isInvisible();

    // Adjust entity information for rendering
    modelData.setModelRootRotation(new CustomRotation(0.0F, 0.0F, 0.0F));
    entity.setInvisible(false);

    // Render Entity
    renderEntity(guiGraphics, x, y, scale, yRot, xRot, easyNPC.getLivingEntity());

    // Restore entity information
    modelData.setModelRootRotation(entityModelRootRotation);
    entity.setInvisible(entityInvisible);
  }

  public static void renderEntityDialog(
      GuiGraphics guiGraphics, int x, int y, float yRot, float xRot, EasyNPC<?> easyNPC) {
    DialogData<?> dialogData = easyNPC.getEasyNPCDialogData();
    renderEntity(
        guiGraphics,
        x,
        y,
        dialogData.getEntityDialogScaling(),
        yRot,
        xRot,
        easyNPC.getLivingEntity());
  }

  public static void renderEntityCustomModel(
      GuiGraphics guiGraphics,
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
        guiGraphics,
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

  public static void renderEntityCustomSkin(
      GuiGraphics guiGraphics,
      int x,
      int y,
      float yRot,
      float xRot,
      EasyNPC<?> easyNPC,
      UUID userUUID,
      SkinType skinType) {
    SkinData<?> skinData = easyNPC.getEasyNPCSkinData();
    GuiData<?> guiData = easyNPC.getEasyNPCGuiData();

    // Backup entity information
    SkinType entitySkinType = skinData.getSkinType();
    UUID entitySkinUUID = skinData.getSkinUUID();

    // Adjust entity information for rendering
    skinData.setSkinDataEntry(skinData.getSkinDataEntry().withType(skinType).withUUID(userUUID));

    // Render Entity
    renderScaledEntityAvatar(
        guiGraphics,
        x + guiData.getEntityGuiLeft(),
        y + guiData.getEntityGuiTop(),
        skinData.getEntitySkinScaling(),
        yRot,
        xRot,
        easyNPC,
        easyNPC.getEasyNPCScaleData(),
        easyNPC.getEasyNPCModelData());

    // Restore entity information
    skinData.setSkinDataEntry(
        skinData.getSkinDataEntry().withType(entitySkinType).withUUID(entitySkinUUID));
  }

  public static void renderEntityDefaultSkin(
      GuiGraphics guiGraphics,
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
    skinData.setSkinDataEntry(
        skinData.getSkinDataEntry().withType(SkinType.DEFAULT).withName("default"));
    variantData.setVariant(variant);
    professionData.setProfession(profession);

    // Render Entity
    renderEntity(
        guiGraphics,
        x + guiData.getEntityGuiLeft(),
        y + guiData.getEntityGuiTop(),
        skinData.getEntitySkinScaling(),
        yRot,
        xRot,
        easyNPC.getLivingEntity());

    // Restore entity information
    skinData.setSkinDataEntry(skinData.getSkinDataEntry().withType(entitySkinType));
    variantData.setVariant(entityVariant);
    professionData.setProfession(entityProfession);
  }
}
