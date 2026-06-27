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

package de.markusbordihn.easynpc.entity.easynpc.data;

import static org.junit.jupiter.api.Assertions.*;

import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogType;
import de.markusbordihn.easynpc.data.model.ModelAnimationBehavior;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.model.RootModelData;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.render.RenderDataEntry;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.data.server.ServerEntityData;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.server.player.FakePlayer;
import java.util.EnumMap;
import net.minecraft.SharedConstants;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.entity.ai.goal.GoalSelector;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

class CustomDataOptimizationTest {

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  @Test
  void testDialogData_emptyTagIsSkippedAndMissingTagKeepsNoneSemantics() {
    TestEasyNPCData source = new TestEasyNPCData();
    CompoundTag savedTag = new CompoundTag();

    source.addAdditionalDialogData(savedTag);

    assertFalse(savedTag.contains(DialogDataCapable.DATA_DIALOG_DATA_TAG));

    TestEasyNPCData target = new TestEasyNPCData();
    target.readAdditionalDialogData(savedTag);

    assertFalse(target.hasDialog());
    assertEquals(DialogType.NONE, target.getDialogDataSet().getType());
  }

  @Test
  void testDialogData_nonEmptyRoundTripPersists() {
    TestEasyNPCData source = new TestEasyNPCData();
    DialogDataEntry dialogDataEntry = new DialogDataEntry("intro", "Guide", "Hello there");
    source.setDialog(dialogDataEntry.getId(), dialogDataEntry);
    CompoundTag savedTag = new CompoundTag();

    source.addAdditionalDialogData(savedTag);

    assertTrue(savedTag.contains(DialogDataCapable.DATA_DIALOG_DATA_TAG));

    TestEasyNPCData target = new TestEasyNPCData();
    target.readAdditionalDialogData(savedTag);

    assertTrue(target.hasDialog("intro"));
  }

  @Test
  void testNavigationData_missingHomeIsSkipped() {
    TestEasyNPCData source = new TestEasyNPCData();
    CompoundTag savedTag = new CompoundTag();

    source.addAdditionalNavigationData(savedTag);

    assertFalse(savedTag.contains(NavigationDataCapable.DATA_NAVIGATION_TAG));

    TestEasyNPCData target = new TestEasyNPCData();
    target.readAdditionalNavigationData(savedTag);

    assertFalse(target.hasHomePosition());
  }

  @Test
  void testRenderData_defaultRenderIsSkippedAndMissingTagUsesDefault() {
    TestEasyNPCData source = new TestEasyNPCData();
    CompoundTag savedTag = new CompoundTag();

    source.addAdditionalRenderData(savedTag);

    assertFalse(savedTag.contains(RenderDataCapable.DATA_RENDER_DATA_TAG));

    TestEasyNPCData target = new TestEasyNPCData();
    target.readAdditionalRenderData(savedTag);

    assertEquals(new RenderDataEntry(), target.getRenderDataEntry());
  }

  @Test
  void testModelData_keepsDefaultPoseButSkipsEmptyChildren() {
    TestEasyNPCData source = new TestEasyNPCData();
    source.setDefaultPose(Pose.CROUCHING);
    source.setModelPartPosition(ModelPartType.HEAD, new CustomPosition(0, 0, 0));
    source.setModelPartScale(ModelPartType.RIGHT_ARM, new CustomScale(1, 1, 1));
    source.setModelPartVisibility(ModelPartType.HELMET, true);
    CompoundTag savedTag = new CompoundTag();

    source.addAdditionalModelData(savedTag);

    assertTrue(savedTag.contains(ModelDataCapable.EASY_NPC_DATA_MODEL_DATA_TAG));
    CompoundTag modelDataTag = savedTag.getCompound(ModelDataCapable.EASY_NPC_DATA_MODEL_DATA_TAG);
    assertEquals(
        Pose.CROUCHING.name(),
        modelDataTag.getString(ModelDataCapable.EASY_NPC_DATA_MODEL_DEFAULT_POSE_TAG));
    assertFalse(modelDataTag.contains(ModelPositionDataCapable.EASY_NPC_DATA_MODEL_POSITION_TAG));
    assertFalse(modelDataTag.contains(ModelScaleDataCapable.EASY_NPC_DATA_MODEL_SCALE_TAG));
    assertFalse(modelDataTag.contains(ModelVisibilityDataCapable.EASY_NPC_DATA_MODEL_VISIBLE_TAG));

    TestEasyNPCData target = new TestEasyNPCData();
    target.readAdditionalModelData(savedTag);

    assertEquals(Pose.CROUCHING, target.getDefaultPose());
  }

  @Test
  void testModelData_nonDefaultValuesRoundTrip() {
    TestEasyNPCData source = new TestEasyNPCData();
    source.setModelPose(ModelPose.CUSTOM);
    source.setModelPoseName("guard_pose");
    source.setModelRootData(
        new RootModelData(
            new CustomRotation(0f, 90f, 0f, true), new CustomScale(1.25f, 1.5f, 1.25f)));
    source.setModelPartPosition(ModelPartType.HEAD, new CustomPosition(1f, 2f, 3f));
    source.setModelPartRotation(ModelPartType.RIGHT_ARM, new CustomRotation(10f, 20f, 30f, true));
    source.setModelPartScale(ModelPartType.LEFT_ARM, new CustomScale(1.1f, 1.2f, 1.3f));
    source.setModelPartVisibility(EquipmentSlot.HEAD, false);
    source.setModelAnimationBehavior(ModelAnimationBehavior.NONE);
    CompoundTag savedTag = new CompoundTag();

    source.addAdditionalModelData(savedTag);

    assertTrue(savedTag.contains(ModelAnimationDataCapable.EASY_NPC_DATA_ANIMATION_DATA_TAG));
    CompoundTag modelDataTag = savedTag.getCompound(ModelDataCapable.EASY_NPC_DATA_MODEL_DATA_TAG);
    assertEquals(
        ModelPose.CUSTOM.name(),
        modelDataTag.getString(ModelDataCapable.EASY_NPC_DATA_MODEL_POSE_TAG));
    assertEquals(
        "guard_pose", modelDataTag.getString(ModelDataCapable.EASY_NPC_DATA_MODEL_POSE_NAME_TAG));
    assertTrue(modelDataTag.contains(ModelRootDataCapable.EASY_NPC_DATA_MODEL_ROOT_TAG));
    assertTrue(modelDataTag.contains(ModelPositionDataCapable.EASY_NPC_DATA_MODEL_POSITION_TAG));
    assertTrue(modelDataTag.contains(ModelRotationDataCapable.EASY_NPC_DATA_MODEL_ROTATION_TAG));
    assertTrue(modelDataTag.contains(ModelScaleDataCapable.EASY_NPC_DATA_MODEL_SCALE_TAG));
    assertTrue(modelDataTag.contains(ModelVisibilityDataCapable.EASY_NPC_DATA_MODEL_VISIBLE_TAG));

    TestEasyNPCData target = new TestEasyNPCData();
    target.readAdditionalModelData(savedTag);

    assertEquals(ModelPose.CUSTOM, target.getModelPose());
    assertEquals("guard_pose", target.getModelPoseName());
    assertEquals(90f, target.getModelRootData().rotation().y());
    assertTrue(target.getModelRootData().rotation().locked());
    assertEquals(1.5f, target.getModelRootData().scale().y());
    assertEquals(1f, target.getModelPartPosition(ModelPartType.HEAD).x());
    assertEquals(20f, target.getModelPartRotation(ModelPartType.RIGHT_ARM).y());
    assertTrue(target.getModelPartRotation(ModelPartType.RIGHT_ARM).locked());
    assertEquals(1.3f, target.getModelPartScale(ModelPartType.LEFT_ARM).z());
    assertFalse(target.getModelPartVisibility(EquipmentSlot.HEAD));
    assertEquals(ModelAnimationBehavior.NONE, target.getModelAnimationBehavior());
  }

  @Test
  void testActionData_emptyStateIsSkipped() {
    TestEasyNPCData source = new TestEasyNPCData();
    CompoundTag savedTag = new CompoundTag();

    source.addAdditionalActionData(savedTag);

    assertFalse(savedTag.contains(ActionEventDataCapable.DATA_ACTION_DATA_TAG));
  }

  @Test
  void testActionData_defaultInteractionActionsRemainPersisted() {
    TestEasyNPCData source = new TestEasyNPCData();
    source.registerDefaultActionInteractionEvents();
    CompoundTag savedTag = new CompoundTag();

    source.addAdditionalActionData(savedTag);

    assertTrue(savedTag.contains(ActionEventDataCapable.DATA_ACTION_DATA_TAG));

    TestEasyNPCData target = new TestEasyNPCData();
    target.readAdditionalActionData(savedTag);

    assertTrue(target.hasActionEvent(ActionEventType.ON_INTERACTION));
  }

  @Test
  void testActionData_nonDefaultPermissionRemainsPersisted() {
    TestEasyNPCData source = new TestEasyNPCData();
    source.setActionPermissionLevel(2);
    CompoundTag savedTag = new CompoundTag();

    source.addAdditionalActionData(savedTag);

    assertTrue(savedTag.contains(ActionEventDataCapable.DATA_ACTION_DATA_TAG));
    assertEquals(
        2,
        savedTag
            .getCompound(ActionEventDataCapable.DATA_ACTION_DATA_TAG)
            .getInt(ActionEventDataCapable.DATA_ACTION_PERMISSION_LEVEL_TAG));

    TestEasyNPCData target = new TestEasyNPCData();
    target.readAdditionalActionData(savedTag);

    assertEquals(2, target.getActionPermissionLevel());
  }

  private static final class TestEasyNPCData
      implements EasyNPC<Mob>,
          ServerDataCapable<Mob>,
          DialogDataCapable<Mob>,
          NavigationDataCapable<Mob>,
          RenderDataCapable<Mob>,
          ActionEventDataCapable<Mob>,
          ModelDataCapable<Mob> {

    private final ServerEntityData serverEntityData = new ServerEntityData(null);
    private final EnumMap<SynchedDataIndex, Object> synchedData =
        new EnumMap<>(SynchedDataIndex.class);
    private Pose defaultPose = Pose.STANDING;
    private int npcDataVersion;

    private TestEasyNPCData() {
      this.defineCustomActionData();
      this.defineCustomDialogData();
      this.defineSynchedNavigationData();
      this.defineSynchedRenderData();
      this.defineSynchedModelData();
    }

    @Override
    public int getNPCDataVersion() {
      return this.npcDataVersion;
    }

    @Override
    public void setNPCDataVersion(int version) {
      this.npcDataVersion = version;
    }

    @Override
    public FakePlayer getFakePlayer(ServerLevel level, BlockPos blockPos) {
      return null;
    }

    @Override
    public boolean isServerSideInstance() {
      return true;
    }

    @Override
    public Pose getDefaultPose() {
      return this.defaultPose;
    }

    @Override
    public void setDefaultPose(Pose pose) {
      this.defaultPose = pose != null ? pose : Pose.STANDING;
    }

    @Override
    public <T> void defineSynchedEntityData(SynchedDataIndex synchedDataIndex, T defaultData) {
      this.synchedData.putIfAbsent(synchedDataIndex, defaultData);
    }

    @Override
    public <T> void setSynchedEntityData(
        SynchedDataIndex synchedDataIndex, T data, boolean forceUpdate) {
      this.synchedData.put(synchedDataIndex, data);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getSynchedEntityData(SynchedDataIndex synchedDataIndex) {
      return (T) this.synchedData.get(synchedDataIndex);
    }

    @Override
    public GoalSelector getEntityGoalSelector() {
      return null;
    }

    @Override
    public GoalSelector getEntityTargetSelector() {
      return null;
    }

    @Override
    public void defineServerEntityData() {}

    @Override
    public ServerEntityData getServerEntityData() {
      return this.serverEntityData;
    }
  }
}
