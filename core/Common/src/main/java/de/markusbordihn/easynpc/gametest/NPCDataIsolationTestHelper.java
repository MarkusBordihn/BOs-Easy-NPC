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

import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.data.display.NameVisibilityType;
import de.markusbordihn.easynpc.data.model.ModelAnimationBehavior;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelRootDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import de.markusbordihn.easynpc.handler.NameHandler;
import de.markusbordihn.easynpc.handler.SkinHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collection;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
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

  private static DisplayAttributeDataCapable<?> requireDisplayData(
      GameTestHelper helper, EasyNPC<?> npc, String label) {
    DisplayAttributeDataCapable<?> data = npc.getEasyNPCDisplayAttributeData();
    if (data == null) {
      helper.fail(label + " display data is null");
    }

    return data;
  }

  private static SkinDataCapable<?> requireSkinData(
      GameTestHelper helper, EasyNPC<?> npc, String label) {
    SkinDataCapable<?> data = npc.getEasyNPCSkinData();
    if (data == null) {
      helper.fail(label + " skin data is null");
    }

    return data;
  }

  private static int drainDirtyEntityData(GameTestHelper helper, EasyNPC<?> npc, String label) {
    try {
      Method packDirtyMethod = npc.getEntity().getEntityData().getClass().getMethod("packDirty");
      Object dirtyValues = packDirtyMethod.invoke(npc.getEntity().getEntityData());
      if (dirtyValues == null) {
        return 0;
      }

      if (dirtyValues instanceof Collection<?> collection) {
        return collection.size();
      }

      helper.fail(label + " dirty entity data is not a collection: " + dirtyValues.getClass());
    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
      helper.fail(label + " failed to inspect dirty entity data: " + e.getMessage());
    }

    return 0;
  }

  private static CompoundTag saveNpcData(EasyNPC<?> npc) {
    return npc.getEntity().saveWithoutId(new CompoundTag());
  }

  public static void assertServerRegistryIsolation(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    EasyNPC<?> serverEasyNPC =
        LivingEntityManager.getServerEasyNPCEntityByUUID(easyNPC.getEntityUUID());
    EasyNPC<?> clientEasyNPC =
        LivingEntityManager.getClientEasyNPCEntityByUUID(easyNPC.getEntityUUID());

    if (serverEasyNPC != easyNPC) {
      helper.fail("Spawned server NPC is missing from the server registry");
    }
    if (clientEasyNPC != null) {
      helper.fail("Spawned server NPC must not be present in the client registry");
    }
    if (LivingEntityManager.getServerEasyNPCEntityByUUID(easyNPC.getEntityUUID()) != easyNPC) {
      helper.fail("Server NPC lookup must resolve the server NPC");
    }
  }

  public static void assertPoseIsolation(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> npc1 = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    EasyNPC<?> npc2 = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 1));

    ModelDataCapable<?> data1 = requireModelData(helper, npc1, "NPC1");
    ModelDataCapable<?> data2 = requireModelData(helper, npc2, "NPC2");
    if (data1 == null || data2 == null) {
      return;
    }

    if (data1.getModelPose() != ModelPose.VANILLA) {
      helper.fail("NPC1 initial pose: expected VANILLA, got " + data1.getModelPose());
    }
    if (data2.getModelPose() != ModelPose.VANILLA) {
      helper.fail("NPC2 initial pose: expected VANILLA, got " + data2.getModelPose());
    }
    data1.setModelPose(ModelPose.CUSTOM);

    if (data1.getModelPose() != ModelPose.CUSTOM) {
      helper.fail("NPC1 pose after change: expected CUSTOM, got " + data1.getModelPose());
    }
    if (data2.getModelPose() != ModelPose.VANILLA) {
      helper.fail("NPC2 pose must not be affected by NPC1 change, got " + data2.getModelPose());
    }
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

  public static void assertNameUpdate(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> npc = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    DisplayAttributeDataCapable<?> displayData = requireDisplayData(helper, npc, "NPC");
    if (displayData == null) {
      return;
    }
    drainDirtyEntityData(helper, npc, "NPC");

    if (!NameHandler.setCustomName(npc, "Ricardo", 0x00FFFF, NameVisibilityType.ALWAYS)) {
      helper.fail("Failed to update NPC name");
    }
    int dirtyCount = drainDirtyEntityData(helper, npc, "NPC");
    CompoundTag savedData = saveNpcData(npc);

    if (npc.getEntity().getCustomName() == null) {
      helper.fail("NPC custom name should not be null after update");
    }
    if (!"Ricardo".equals(npc.getEntity().getCustomName().getString())) {
      helper.fail(
          "NPC custom name after update: expected Ricardo, got "
              + npc.getEntity().getCustomName().getString());
    }
    if (!npc.getEntity().isCustomNameVisible()) {
      helper.fail("NPC custom name should be visible after update");
    }

    NameVisibilityType nameVisibilityType =
        displayData.getDisplayEnumAttribute(
            DisplayAttributeType.NAME_VISIBILITY, NameVisibilityType.class);
    if (nameVisibilityType != NameVisibilityType.ALWAYS) {
      helper.fail("NPC name visibility after update: expected ALWAYS, got " + nameVisibilityType);
    }
    if (dirtyCount <= 0) {
      helper.fail("NPC name update should mark synced entity data as dirty");
    }
    if (!savedData.contains("CustomName")) {
      helper.fail("NPC saved data should contain CustomName after name update");
    }
    if (!savedData.contains("CustomNameVisible") || !savedData.getBoolean("CustomNameVisible")) {
      helper.fail("NPC saved data should contain CustomNameVisible=true after name update");
    }
  }

  public static void assertSkinUpdate(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> npc = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    SkinDataCapable<?> skinData = requireSkinData(helper, npc, "NPC");
    VariantDataCapable<?> variantData = npc.getEasyNPCVariantData();
    if (skinData == null || variantData == null) {
      return;
    }
    drainDirtyEntityData(helper, npc, "NPC");

    if (!SkinHandler.setSkin(npc, SkinDataEntry.createDefaultSkin("ALEX"))) {
      helper.fail("Failed to update NPC skin");
    }
    int dirtyCount = drainDirtyEntityData(helper, npc, "NPC");
    CompoundTag savedData = saveNpcData(npc);

    SkinDataEntry skinDataEntry = skinData.getSkinDataEntry();
    if (skinDataEntry.type() != SkinType.DEFAULT) {
      helper.fail("NPC skin type after update: expected DEFAULT, got " + skinDataEntry.type());
    }
    if (!"ALEX".equals(skinDataEntry.name())) {
      helper.fail("NPC skin name after update: expected ALEX, got " + skinDataEntry.name());
    }
    if (!"ALEX".equals(variantData.getSkinVariantType().name())) {
      helper.fail(
          "NPC variant after skin update: expected ALEX, got "
              + variantData.getSkinVariantType().name());
    }
    if (dirtyCount <= 0) {
      helper.fail("NPC skin update should mark synced entity data as dirty");
    }
    if (!savedData.contains(SkinDataCapable.EASY_NPC_DATA_SKIN_DATA_TAG)) {
      helper.fail("NPC saved data should contain SkinData after skin update");
    }
    CompoundTag skinTag = savedData.getCompound(SkinDataCapable.EASY_NPC_DATA_SKIN_DATA_TAG);
    if (!"ALEX".equals(skinTag.getString("Name"))) {
      helper.fail(
          "NPC saved skin name after update: expected ALEX, got " + skinTag.getString("Name"));
    }
  }

  public static void assertScaleUpdatesDimensions(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> npc = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    ModelDataCapable<?> modelData = requireModelData(helper, npc, "NPC");
    if (modelData == null) {
      return;
    }

    drainDirtyEntityData(helper, npc, "NPC");
    float initialHeight = npc.getEntity().getDimensions(npc.getEntity().getPose()).height;
    CustomScale doubleScale = new CustomScale(2f, 2f, 2f);
    modelData.setModelRootScale(doubleScale);
    float updatedHeight = npc.getEntity().getDimensions(npc.getEntity().getPose()).height;
    int dirtyCount = drainDirtyEntityData(helper, npc, "NPC");
    CompoundTag savedData = saveNpcData(npc);

    if (!doubleScale.equals(modelData.getModelRootData().scale())) {
      helper.fail(
          "NPC ROOT scale after update: expected "
              + doubleScale
              + ", got "
              + modelData.getModelRootData().scale());
    }
    if (updatedHeight <= initialHeight) {
      helper.fail(
          "NPC height after scaling should increase, got "
              + updatedHeight
              + " from "
              + initialHeight);
    }
    if (dirtyCount <= 0) {
      helper.fail("NPC scale update should mark synced entity data as dirty");
    }
    if (!savedData.contains(ModelDataCapable.EASY_NPC_DATA_MODEL_DATA_TAG)) {
      helper.fail("NPC saved data should contain ModelData after scale update");
    }
    CompoundTag modelTag = savedData.getCompound(ModelDataCapable.EASY_NPC_DATA_MODEL_DATA_TAG);
    if (!modelTag.contains(ModelRootDataCapable.EASY_NPC_DATA_MODEL_ROOT_TAG)) {
      helper.fail("NPC saved model data should contain Root after scale update");
    }
    CompoundTag rootTag = modelTag.getCompound(ModelRootDataCapable.EASY_NPC_DATA_MODEL_ROOT_TAG);
    if (!rootTag.contains("Scale")) {
      helper.fail("NPC saved root model data should contain Scale after scale update");
    }
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
