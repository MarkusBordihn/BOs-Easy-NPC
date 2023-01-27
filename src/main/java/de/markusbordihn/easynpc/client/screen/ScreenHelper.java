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

import com.mojang.blaze3d.platform.Lighting;
import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Quaternion;
import com.mojang.math.Vector3f;

import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRenderDispatcher;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;

public class ScreenHelper {

  public static void renderEntity(int x, int y, float yRot, float xRot, EasyNPCEntity entity) {
    float f = (float) Math.atan(yRot / 40.0F);
    float f1 = (float) Math.atan(xRot / 40.0F);
    int scale = 50;
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
    float entityYBodyRot = entity.yBodyRot;
    float entityYRot = entity.getYRot();
    float entityXRot = entity.getXRot();
    float entityYHeadRotO = entity.yHeadRotO;
    float entityYHeadRot = entity.yHeadRot;
    entity.yBodyRot = 180.0F + f * 20.0F;
    entity.setYRot(180.0F + f * 40.0F);
    entity.setXRot(-f1 * 20.0F);
    entity.yHeadRot = entity.getYRot();
    Component customName = entity.getCustomName();
    entity.setCustomName(null);
    Lighting.setupForEntityInInventory();
    EntityRenderDispatcher entityRenderDispatcher =
        Minecraft.getInstance().getEntityRenderDispatcher();
    quaternion1.conj();
    entityRenderDispatcher.overrideCameraOrientation(quaternion1);
    entityRenderDispatcher.setRenderShadow(false);
    MultiBufferSource.BufferSource multiBuffer =
        Minecraft.getInstance().renderBuffers().bufferSource();
    entityRenderDispatcher.render(entity, 0.0D, 0.0D, 0.0D, 0.0F, 1.0F, poseStack1, multiBuffer,
        15728880);
    multiBuffer.endBatch();
    entityRenderDispatcher.setRenderShadow(true);
    entity.yBodyRot = entityYBodyRot;
    entity.setYRot(entityYRot);
    entity.setXRot(entityXRot);
    entity.yHeadRot = entityYHeadRot;
    entity.yHeadRotO = entityYHeadRotO;
    entity.setCustomName(customName);
    poseStack.popPose();
    RenderSystem.applyModelViewMatrix();
    Lighting.setupFor3DItems();
  }

  public static void renderEntityAvatar(int x, int y, float yRot, float xRot,
      EasyNPCEntity entity) {
    float f = (float) Math.atan(yRot / 40.0F);
    float f1 = (float) Math.atan(xRot / 40.0F);
    int scale = 50;
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
    float entityYBodyRot = entity.yBodyRot;
    float entityYRot = entity.getYRot();
    float entityXRot = entity.getXRot();
    float entityYHeadRotO = entity.yHeadRotO;
    float entityYHeadRot = entity.yHeadRot;
    entity.yBodyRot = 180.0F + f * 20.0F;
    entity.setYRot(180.0F + f * 40.0F);
    entity.setXRot(-f1 * 20.0F);
    entity.yHeadRot = entity.getYRot();
    Component customName = entity.getCustomName();
    entity.setCustomName(null);
    Lighting.setupForEntityInInventory();
    EntityRenderDispatcher entityRenderDispatcher =
        Minecraft.getInstance().getEntityRenderDispatcher();
    quaternion1.conj();
    entityRenderDispatcher.overrideCameraOrientation(quaternion1);
    entityRenderDispatcher.setRenderShadow(false);
    MultiBufferSource.BufferSource multiBuffer =
        Minecraft.getInstance().renderBuffers().bufferSource();
    entityRenderDispatcher.render(entity, 0.0D, 0.0D, 0.0D, 0.0F, 1.0F, poseStack1, multiBuffer,
        15728880);
    multiBuffer.endBatch();
    entityRenderDispatcher.setRenderShadow(true);
    entity.yBodyRot = entityYBodyRot;
    entity.setYRot(entityYRot);
    entity.setXRot(entityXRot);
    entity.yHeadRot = entityYHeadRot;
    entity.yHeadRotO = entityYHeadRotO;
    entity.setCustomName(customName);
    poseStack.popPose();
    RenderSystem.applyModelViewMatrix();
    Lighting.setupFor3DItems();
  }

  public static void renderEntitySkin(int x, int y, float yRot, float xRot, EasyNPCEntity entity,
      ResourceLocation variantTextureLocation, ResourceLocation professionTextureLocation) {
    float f = (float) Math.atan(yRot / 40.0F);
    float f1 = (float) Math.atan(xRot / 40.0F);
    int scale = 30;
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
    float entityYBodyRot = entity.yBodyRot;
    float entityYRot = entity.getYRot();
    float entityXRot = entity.getXRot();
    float entityYHeadRotO = entity.yHeadRotO;
    float entityYHeadRot = entity.yHeadRot;
    ResourceLocation entityTextureLocation = entity.getTextureLocation();
    ResourceLocation entityProfessionTextureLocation =
        professionTextureLocation != null ? entity.getProfessionTextureLocation() : null;

    // Adjust entity information for rendering
    entity.yBodyRot = 180.0F + f * 20.0F;
    entity.setYRot(180.0F + f * 40.0F);
    entity.setXRot(-f1 * 20.0F);
    entity.yHeadRot = entity.getYRot();
    entity.setTextureLocation(variantTextureLocation);
    if (professionTextureLocation != null) {
      entity.setProfessionTextureLocation(professionTextureLocation);
    }
    Component customName = entity.getCustomName();
    entity.setCustomName(null);
    Lighting.setupForEntityInInventory();
    EntityRenderDispatcher entityRenderDispatcher =
        Minecraft.getInstance().getEntityRenderDispatcher();
    quaternion1.conj();
    entityRenderDispatcher.overrideCameraOrientation(quaternion1);
    entityRenderDispatcher.setRenderShadow(false);
    MultiBufferSource.BufferSource multiBuffer =
        Minecraft.getInstance().renderBuffers().bufferSource();
    entityRenderDispatcher.render(entity, 0.0D, 0.0D, 0.0D, 0.0F, 1.0F, poseStack1, multiBuffer,
        15728880);
    multiBuffer.endBatch();
    entityRenderDispatcher.setRenderShadow(true);

    // Restore entity information
    entity.yBodyRot = entityYBodyRot;
    entity.setYRot(entityYRot);
    entity.setXRot(entityXRot);
    entity.yHeadRot = entityYHeadRot;
    entity.yHeadRotO = entityYHeadRotO;
    entity.setCustomName(customName);
    entity.setTextureLocation(entityTextureLocation);
    if (entityProfessionTextureLocation != null) {
      entity.setProfessionTextureLocation(entityProfessionTextureLocation);
    }
    poseStack.popPose();
    RenderSystem.applyModelViewMatrix();
    Lighting.setupFor3DItems();
  }

}
