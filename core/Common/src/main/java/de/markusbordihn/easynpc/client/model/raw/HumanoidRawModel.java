/*
 * Copyright 2025 Markus Bordihn
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

package de.markusbordihn.easynpc.client.model.raw;

import de.markusbordihn.easynpc.client.model.EasyNPCModel;
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.data.model.ModelArmPose;
import de.markusbordihn.easynpc.data.model.ModelArmPoseHelper;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ArmPoseProvider;
import net.minecraft.client.model.HumanoidModel.ArmPose;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.renderer.entity.state.HumanoidRenderState;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.HumanoidArm;

public class HumanoidRawModel {

  public static <S extends HumanoidRenderState> ModelArmPose getCustomArmPose(
      S renderState, HumanoidArm humanoidArm) {
    if (renderState instanceof EasyNPCRenderStateExtension extension) {
      return humanoidArm == HumanoidArm.RIGHT
          ? extension.getEasyNpcRightArmPose()
          : extension.getEasyNpcLeftArmPose();
    }
    return ModelArmPose.DEFAULT;
  }

  public static <S extends HumanoidRenderState> ArmPose getArmPose(
      EasyNPCRenderStateExtension extension, S renderState, HumanoidArm humanoidArm) {
    if (extension == null || renderState == null || humanoidArm == null) {
      return ArmPose.EMPTY;
    }

    // Get EasyNPC
    EasyNPC<?> easyNPC = EasyNPCModel.getEasyNPC(extension);
    if (easyNPC == null) {
      return ArmPose.EMPTY;
    }

    // Use the new ArmPoseProvider interface if available
    if (easyNPC instanceof ArmPoseProvider armPoseProvider) {
      return ModelArmPoseHelper.toMinecraftArmPose(armPoseProvider.getArmPose(humanoidArm));
    }

    // Fallback to default behavior
    return ArmPose.ITEM;
  }

  public static void applyArmPosesToModel(
      ModelArmPose rightArmPose,
      ModelArmPose leftArmPose,
      ModelPart rightArm,
      ModelPart leftArm,
      ModelPart head) {

    // Apply right arm pose
    if (rightArmPose != null && rightArmPose != ModelArmPose.DEFAULT) {
      applyArmPoseToModelPart(rightArmPose, rightArm, true, head);
    }

    // Apply left arm pose
    if (leftArmPose != null && leftArmPose != ModelArmPose.DEFAULT) {
      applyArmPoseToModelPart(leftArmPose, leftArm, false, head);
    }
  }

  private static void applyArmPoseToModelPart(
      ModelArmPose armPose, ModelPart armModelPart, boolean isRightArm, ModelPart head) {
    if (armPose == null || armModelPart == null) {
      return;
    }

    switch (armPose) {
      case BOW_AND_ARROW -> {
        if (isRightArm) {
          armModelPart.yRot = -0.1F + head.yRot;
        } else {
          armModelPart.yRot = 0.1F + head.yRot + 0.4F;
        }
        armModelPart.xRot = (float) (-Math.PI / 2) + head.xRot;
      }
      case CROSSBOW_HOLD -> {
        if (isRightArm) {
          armModelPart.yRot = -0.3F + head.yRot;
          armModelPart.xRot = (float) (-Math.PI / 2) + head.xRot + 0.1F;
        } else {
          armModelPart.yRot = 0.6F + head.yRot;
          armModelPart.xRot = -1.5F + head.xRot;
        }
      }
      case CROSSBOW_CHARGE -> {
        if (isRightArm) {
          armModelPart.yRot = -0.8F;
          armModelPart.xRot = -0.97079635F;
        } else {
          armModelPart.yRot = 0.85F;
          armModelPart.xRot = (float) (-Math.PI / 2);
        }
      }
      case SPYGLASS -> {
        armModelPart.xRot = Mth.clamp(armModelPart.xRot, -1.2F, 1.2F) - 1.9198622F;
        armModelPart.yRot = isRightArm ? 0.5235988F : -0.5235988F;
      }
      case ATTACKING_WITH_MELEE_WEAPON -> {
        // Swing animation for melee weapons
        float swingProgress = 0.5F;
        float rotation = Mth.sin(swingProgress * (float) Math.PI);
        armModelPart.xRot = rotation * -1.2F;
        armModelPart.yRot = isRightArm ? -0.3F : 0.3F;
      }
      case SPELLCASTING -> {
        armModelPart.xRot = armModelPart.xRot * 0.5F - (float) Math.PI;
        armModelPart.yRot = 0.0F;
      }
      // For other poses, keep default positioning
      default -> {
        // No special positioning needed
      }
    }
  }
}
