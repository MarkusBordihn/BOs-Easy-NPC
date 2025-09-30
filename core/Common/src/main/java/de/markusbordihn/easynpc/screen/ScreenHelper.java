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

import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.profession.Profession;
import de.markusbordihn.easynpc.data.render.RenderDataSet;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.GuiDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ProfessionDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import java.util.UUID;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.screens.inventory.InventoryScreen;
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
    boolean isDead = livingEntity.isDeadOrDying();
    Minecraft minecraft = Minecraft.getInstance();
    float rotationY = (float) Math.atan((isDead ? 25F : yRot) / 40.0F);
    float rotationX = (float) Math.atan((isDead ? -25F : xRot) / 40.0F);
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
    // Note: Matrix3x2fStack doesn't support pushPose/popPose or 3D operations in 1.21.8
    // Using simplified rendering approach
    if (isDead) {
      guiGraphics.pose().translate((float) (x - 25.0D), (float) (y - 30.0D));
    } else {
      guiGraphics.pose().translate((float) x, (float) y);
    }
    guiGraphics.pose().scale(scale, scale);

    EntityRenderDispatcher entityRenderDispatcher =
        Minecraft.getInstance().getEntityRenderDispatcher();
    quaternionfX.conjugate();
    entityRenderDispatcher.overrideCameraOrientation(quaternionfX);
    entityRenderDispatcher.setRenderShadow(false);

    // Direct entity rendering without drawSpecial/flush
    InventoryScreen.renderEntityInInventoryFollowsMouse(
        guiGraphics, 0, 0, 0, 0, 0, 0.0F, 0.0F, 0.0F, livingEntity);

    entityRenderDispatcher.setRenderShadow(true);
    // Reset transformations
    guiGraphics.pose().scale(1.0f / scale, 1.0f / scale);
    if (isDead) {
      guiGraphics.pose().translate((float) (-(x - 25.0D)), (float) (-(y - 30.0D)));
    } else {
      guiGraphics.pose().translate((float) (-x), (float) (-y));
    }

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
    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData != null) {
      renderScaledEntityAvatar(guiGraphics, x, y, scale, yRot, xRot, easyNPC, modelData);
    } else {
      renderEntity(guiGraphics, x, y, scale, yRot, xRot, easyNPC.getLivingEntity());
    }
  }

  public static void renderScaledEntityAvatar(
      GuiGraphics guiGraphics, int x, int y, float yRot, float xRot, EasyNPC<?> easyNPC) {
    GuiDataCapable<?> guiData = easyNPC.getEasyNPCGuiData();
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
      ModelDataCapable<?> modelData) {

    // Backup entity information
    CustomRotation entityModelRootRotation =
        modelData != null ? modelData.getModelPartRotation(ModelPartType.ROOT) : null;
    CustomScale entityModelRootScale =
        modelData != null ? modelData.getModelPartScale(ModelPartType.ROOT) : null;

    // Adjust entity information for rendering
    if (modelData != null) {
      modelData.setModelPartRotation(ModelPartType.ROOT, new CustomRotation(0.0F, 0.0F, 0.0F));
      modelData.setModelPartScale(ModelPartType.ROOT, new CustomScale(1.0F, 1.0F, 1.0F));
    }

    // Render Entity
    renderEntity(guiGraphics, x, y, scale, yRot, xRot, easyNPC.getLivingEntity());

    // Restore entity information
    if (modelData != null) {
      if (entityModelRootRotation != null) {
        modelData.setModelPartRotation(ModelPartType.ROOT, entityModelRootRotation);
      }
      if (entityModelRootScale != null) {
        modelData.setModelPartScale(ModelPartType.ROOT, entityModelRootScale);
      }
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
    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    Entity entity = easyNPC.getEntity();

    // Backup entity information
    ModelPose entityModelPose = modelData.getModelPose();
    Pose entityPose = easyNPC.getEntity().getPose();

    // Adjust entity information for rendering
    modelData.setModelPose(ModelPose.CUSTOM);
    entity.setPose(Pose.STANDING);

    // Render Entity
    renderScaledEntityAvatar(guiGraphics, x, y, scale, yRot, xRot, easyNPC, modelData);

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
    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    Entity entity = easyNPC.getEntity();

    // Backup entity information
    CustomRotation entityModelRootRotation = modelData.getModelPartRotation(ModelPartType.ROOT);
    boolean entityInvisible = entity.isInvisible();

    // Adjust entity information for rendering
    modelData.setModelPartRotation(ModelPartType.ROOT, new CustomRotation(0.0F, 0.0F, 0.0F));
    entity.setInvisible(false);

    // Render Entity
    renderEntity(guiGraphics, x, y, scale, yRot, xRot, easyNPC.getLivingEntity());

    // Restore entity information
    modelData.setModelPartRotation(ModelPartType.ROOT, entityModelRootRotation);
    entity.setInvisible(entityInvisible);
  }

  public static void renderEntityDialog(
      GuiGraphics guiGraphics, int x, int y, float yRot, float xRot, EasyNPC<?> easyNPC) {
    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();

    // Backup entity information
    CustomRotation entityModelRootRotation = modelData.getModelPartRotation(ModelPartType.ROOT);

    // Adjust entity information for rendering
    modelData.setModelPartRotation(ModelPartType.ROOT, new CustomRotation(0.0F, 0.0F, 0.0F));

    // Render Entity
    DialogDataCapable<?> dialogData = easyNPC.getEasyNPCDialogData();
    renderEntity(
        guiGraphics,
        x,
        y,
        dialogData.getEntityDialogScaling(),
        yRot,
        xRot,
        easyNPC.getLivingEntity());

    // Restore entity information
    modelData.setModelPartRotation(ModelPartType.ROOT, entityModelRootRotation);
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
    GuiDataCapable<?> guiData = easyNPC.getEasyNPCGuiData();
    RenderDataCapable<?> renderData = easyNPC.getEasyNPCRenderData();
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
    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    GuiDataCapable<?> guiData = easyNPC.getEasyNPCGuiData();

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
    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    VariantDataCapable<?> variantData = easyNPC.getEasyNPCVariantData();
    ProfessionDataCapable<?> professionData = easyNPC.getEasyNPCProfessionData();
    GuiDataCapable<?> guiData = easyNPC.getEasyNPCGuiData();

    // Backup entity information
    SkinType entitySkinType = skinData.getSkinType();
    Enum<?> entityVariant = variantData.getVariantType();
    Profession entityProfession = professionData.getProfession();

    // Adjust entity information for rendering
    skinData.setSkinDataEntry(
        skinData.getSkinDataEntry().withType(SkinType.DEFAULT).withName("default"));
    variantData.setVariantType(variant);
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
    variantData.setVariantType(entityVariant);
    professionData.setProfession(entityProfession);
  }
}
