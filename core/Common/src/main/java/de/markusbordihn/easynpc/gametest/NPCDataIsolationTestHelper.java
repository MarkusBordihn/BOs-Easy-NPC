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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.data.model.ModelAnimationBehavior;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class NPCDataIsolationTestHelper {

  private NPCDataIsolationTestHelper() {}

  private static ModelDataCapable<?> requireModelData(
      GameTestHelper helper, EasyNPC<?> npc, String label) {
    ModelDataCapable<?> data = npc.getEasyNPCModelData();
    if (data == null) {
      helper.fail(label + " model data is null");
    }
    return data;
  }

  public static void assertPoseIsolation(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> npc1 = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    EasyNPC<?> npc2 = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 1));

    ModelDataCapable<?> data1 = requireModelData(helper, npc1, "NPC1");
    ModelDataCapable<?> data2 = requireModelData(helper, npc2, "NPC2");
    if (data1 == null || data2 == null) return;

    if (data1.getModelPose() != ModelPose.VANILLA)
      helper.fail("NPC1 initial pose: expected VANILLA, got " + data1.getModelPose());
    if (data2.getModelPose() != ModelPose.VANILLA)
      helper.fail("NPC2 initial pose: expected VANILLA, got " + data2.getModelPose());

    data1.setModelPose(ModelPose.CUSTOM);

    if (data1.getModelPose() != ModelPose.CUSTOM)
      helper.fail("NPC1 pose after change: expected CUSTOM, got " + data1.getModelPose());
    if (data2.getModelPose() != ModelPose.VANILLA)
      helper.fail("NPC2 pose must not be affected by NPC1 change, got " + data2.getModelPose());
  }

  public static void assertPoseNameIsolation(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> npc1 = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    EasyNPC<?> npc2 = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 1));

    ModelDataCapable<?> data1 = requireModelData(helper, npc1, "NPC1");
    ModelDataCapable<?> data2 = requireModelData(helper, npc2, "NPC2");
    if (data1 == null || data2 == null) return;

    if (!data1.getModelPoseName().isEmpty())
      helper.fail("NPC1 initial pose name should be empty, got " + data1.getModelPoseName());
    if (!data2.getModelPoseName().isEmpty())
      helper.fail("NPC2 initial pose name should be empty, got " + data2.getModelPoseName());

    data1.setModelPoseName("custom_red_name");

    if (!"custom_red_name".equals(data1.getModelPoseName()))
      helper.fail(
          "NPC1 pose name after change: expected custom_red_name, got " + data1.getModelPoseName());
    if (!data2.getModelPoseName().isEmpty())
      helper.fail(
          "NPC2 pose name must not be affected by NPC1 change, got " + data2.getModelPoseName());
  }

  public static void assertRotationIsolation(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> npc1 = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    EasyNPC<?> npc2 = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 1));

    ModelDataCapable<?> data1 = requireModelData(helper, npc1, "NPC1");
    ModelDataCapable<?> data2 = requireModelData(helper, npc2, "NPC2");
    if (data1 == null || data2 == null) return;

    CustomRotation defaultRotation = new CustomRotation(0f, 0f, 0f);
    CustomRotation liftedArmRotation = new CustomRotation(1.5f, 0f, 0f);

    if (!defaultRotation.equals(data1.getModelPartRotation(ModelPartType.RIGHT_ARM)))
      helper.fail(
          "NPC1 initial right-arm rotation: expected "
              + defaultRotation
              + ", got "
              + data1.getModelPartRotation(ModelPartType.RIGHT_ARM));
    if (!defaultRotation.equals(data2.getModelPartRotation(ModelPartType.RIGHT_ARM)))
      helper.fail(
          "NPC2 initial right-arm rotation: expected "
              + defaultRotation
              + ", got "
              + data2.getModelPartRotation(ModelPartType.RIGHT_ARM));

    data1.setModelPartRotation(ModelPartType.RIGHT_ARM, liftedArmRotation);

    if (!liftedArmRotation.equals(data1.getModelPartRotation(ModelPartType.RIGHT_ARM)))
      helper.fail(
          "NPC1 right-arm rotation after change: expected "
              + liftedArmRotation
              + ", got "
              + data1.getModelPartRotation(ModelPartType.RIGHT_ARM));
    if (!defaultRotation.equals(data2.getModelPartRotation(ModelPartType.RIGHT_ARM)))
      helper.fail(
          "NPC2 right-arm rotation must not be affected by NPC1 change, got "
              + data2.getModelPartRotation(ModelPartType.RIGHT_ARM));
  }

  public static void assertAnimationIsolation(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> npc1 = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    EasyNPC<?> npc2 = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 1));

    ModelDataCapable<?> data1 = requireModelData(helper, npc1, "NPC1");
    ModelDataCapable<?> data2 = requireModelData(helper, npc2, "NPC2");
    if (data1 == null || data2 == null) return;

    if (data1.getModelAnimationBehavior() != ModelAnimationBehavior.SMART)
      helper.fail(
          "NPC1 initial animation behavior: expected SMART, got "
              + data1.getModelAnimationBehavior());
    if (data2.getModelAnimationBehavior() != ModelAnimationBehavior.SMART)
      helper.fail(
          "NPC2 initial animation behavior: expected SMART, got "
              + data2.getModelAnimationBehavior());

    data1.setModelAnimationBehavior(ModelAnimationBehavior.NONE);

    if (data1.getModelAnimationBehavior() != ModelAnimationBehavior.NONE)
      helper.fail(
          "NPC1 animation behavior after change: expected NONE, got "
              + data1.getModelAnimationBehavior());
    if (data2.getModelAnimationBehavior() != ModelAnimationBehavior.SMART)
      helper.fail(
          "NPC2 animation behavior must not be affected by NPC1 change, got "
              + data2.getModelAnimationBehavior());
  }

  public static void assertScaleIsolation(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> npc1 = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    EasyNPC<?> npc2 = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 1));

    ModelDataCapable<?> data1 = requireModelData(helper, npc1, "NPC1");
    ModelDataCapable<?> data2 = requireModelData(helper, npc2, "NPC2");
    if (data1 == null || data2 == null) return;

    CustomScale defaultScale = new CustomScale(1f, 1f, 1f);
    CustomScale doubleScale = new CustomScale(2f, 2f, 2f);

    if (!defaultScale.equals(data1.getModelRootData().scale()))
      helper.fail(
          "NPC1 initial ROOT scale: expected "
              + defaultScale
              + ", got "
              + data1.getModelRootData().scale());
    if (!defaultScale.equals(data2.getModelRootData().scale()))
      helper.fail(
          "NPC2 initial ROOT scale: expected "
              + defaultScale
              + ", got "
              + data2.getModelRootData().scale());

    data1.setModelRootScale(doubleScale);

    if (!doubleScale.equals(data1.getModelRootData().scale()))
      helper.fail(
          "NPC1 ROOT scale after change: expected "
              + doubleScale
              + ", got "
              + data1.getModelRootData().scale());
    if (!defaultScale.equals(data2.getModelRootData().scale()))
      helper.fail(
          "NPC2 ROOT scale must not be affected by NPC1 change, got "
              + data2.getModelRootData().scale());
  }

  public static void assertRootDataIsolation(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> npc1 = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    EasyNPC<?> npc2 = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 1));

    ModelDataCapable<?> data1 = requireModelData(helper, npc1, "NPC1");
    ModelDataCapable<?> data2 = requireModelData(helper, npc2, "NPC2");
    if (data1 == null || data2 == null) return;

    // Both NPCs must start unlocked.
    if (data1.getModelRootData().isRotationLocked())
      helper.fail("NPC1 must start with an unlocked root rotation");
    if (data2.getModelRootData().isRotationLocked())
      helper.fail("NPC2 must start with an unlocked root rotation");

    // Lock NPC1 root rotation to 90° Y and verify NPC2 is unaffected.
    data1.setModelRootRotation(new CustomRotation(0f, 90f, 0f, true));

    if (!data1.getModelRootData().isRotationLocked())
      helper.fail("NPC1 root rotation must be locked after setModelRootRotation");
    if (data1.getModelRootData().rotation().y() != 90f)
      helper.fail(
          "NPC1 root rotation Y: expected 90, got " + data1.getModelRootData().rotation().y());
    if (data2.getModelRootData().isRotationLocked())
      helper.fail("NPC2 root rotation must not be affected by NPC1 change");
  }

  public static void assertPositionIsolation(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> npc1 = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    EasyNPC<?> npc2 = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 1));

    ModelDataCapable<?> data1 = requireModelData(helper, npc1, "NPC1");
    ModelDataCapable<?> data2 = requireModelData(helper, npc2, "NPC2");
    if (data1 == null || data2 == null) return;

    CustomPosition defaultPosition = new CustomPosition(0f, 0f, 0f);
    CustomPosition shiftedPosition = new CustomPosition(0f, 1f, 0f);

    if (!defaultPosition.equals(data1.getModelPartPosition(ModelPartType.ROOT)))
      helper.fail(
          "NPC1 initial ROOT position: expected "
              + defaultPosition
              + ", got "
              + data1.getModelPartPosition(ModelPartType.ROOT));
    if (!defaultPosition.equals(data2.getModelPartPosition(ModelPartType.ROOT)))
      helper.fail(
          "NPC2 initial ROOT position: expected "
              + defaultPosition
              + ", got "
              + data2.getModelPartPosition(ModelPartType.ROOT));

    data1.setModelPartPosition(ModelPartType.ROOT, shiftedPosition);

    if (!shiftedPosition.equals(data1.getModelPartPosition(ModelPartType.ROOT)))
      helper.fail(
          "NPC1 ROOT position after change: expected "
              + shiftedPosition
              + ", got "
              + data1.getModelPartPosition(ModelPartType.ROOT));
    if (!defaultPosition.equals(data2.getModelPartPosition(ModelPartType.ROOT)))
      helper.fail(
          "NPC2 ROOT position must not be affected by NPC1 change, got "
              + data2.getModelPartPosition(ModelPartType.ROOT));
  }

  public static void assertVisibilityIsolation(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> npc1 = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    EasyNPC<?> npc2 = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 1));

    ModelDataCapable<?> data1 = requireModelData(helper, npc1, "NPC1");
    ModelDataCapable<?> data2 = requireModelData(helper, npc2, "NPC2");
    if (data1 == null || data2 == null) return;

    if (!data1.getModelPartVisibility(ModelPartType.RIGHT_ARM))
      helper.fail("NPC1 initial right-arm visibility should be true");
    if (!data2.getModelPartVisibility(ModelPartType.RIGHT_ARM))
      helper.fail("NPC2 initial right-arm visibility should be true");

    data1.setModelPartVisibility(ModelPartType.RIGHT_ARM, false);

    if (data1.getModelPartVisibility(ModelPartType.RIGHT_ARM))
      helper.fail("NPC1 right-arm should now be hidden");
    if (!data2.getModelPartVisibility(ModelPartType.RIGHT_ARM))
      helper.fail("NPC2 right-arm must not be affected by NPC1 change");
  }
}
