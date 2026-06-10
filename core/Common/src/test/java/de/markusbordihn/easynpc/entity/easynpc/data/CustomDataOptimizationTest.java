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
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.render.RenderDataEntry;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.data.server.ServerEntityData;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.server.player.FakePlayer;
import java.util.EnumMap;
import net.minecraft.SharedConstants;
import net.minecraft.core.BlockPos;
import net.minecraft.core.RegistryAccess;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.ProblemReporter;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.entity.ai.goal.GoalSelector;
import net.minecraft.world.level.storage.TagValueInput;
import net.minecraft.world.level.storage.TagValueOutput;
import net.minecraft.world.level.storage.ValueInput;
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
    TagValueOutput valueOutput = TagValueOutput.createWithoutContext(ProblemReporter.DISCARDING);

    source.addAdditionalDialogData(valueOutput);

    CompoundTag savedTag = valueOutput.buildResult();
    assertFalse(savedTag.contains(DialogDataCapable.DATA_DIALOG_DATA_TAG));

    TestEasyNPCData target = new TestEasyNPCData();
    ValueInput valueInput =
        TagValueInput.create(ProblemReporter.DISCARDING, RegistryAccess.EMPTY, savedTag);
    target.readAdditionalDialogData(valueInput);

    assertFalse(target.hasDialog());
    assertEquals(DialogType.NONE, target.getDialogDataSet().getType());
  }

  @Test
  void testDialogData_nonEmptyRoundTripPersists() {
    TestEasyNPCData source = new TestEasyNPCData();
    DialogDataEntry dialogDataEntry = new DialogDataEntry("intro", "Guide", "Hello there");
    source.setDialog(dialogDataEntry.getId(), dialogDataEntry);
    TagValueOutput valueOutput = TagValueOutput.createWithoutContext(ProblemReporter.DISCARDING);

    source.addAdditionalDialogData(valueOutput);

    CompoundTag savedTag = valueOutput.buildResult();
    assertTrue(savedTag.contains(DialogDataCapable.DATA_DIALOG_DATA_TAG));

    TestEasyNPCData target = new TestEasyNPCData();
    ValueInput valueInput =
        TagValueInput.create(ProblemReporter.DISCARDING, RegistryAccess.EMPTY, savedTag);
    target.readAdditionalDialogData(valueInput);

    assertTrue(target.hasDialog("intro"));
  }

  @Test
  void testNavigationData_missingHomeIsSkipped() {
    TestEasyNPCData source = new TestEasyNPCData();
    TagValueOutput valueOutput = TagValueOutput.createWithoutContext(ProblemReporter.DISCARDING);

    source.addAdditionalNavigationData(valueOutput);

    CompoundTag savedTag = valueOutput.buildResult();
    assertFalse(savedTag.contains(NavigationDataCapable.DATA_NAVIGATION_TAG));

    TestEasyNPCData target = new TestEasyNPCData();
    ValueInput valueInput =
        TagValueInput.create(ProblemReporter.DISCARDING, RegistryAccess.EMPTY, savedTag);
    target.readAdditionalNavigationData(valueInput);

    assertFalse(target.hasHomePosition());
  }

  @Test
  void testRenderData_defaultRenderIsSkippedAndMissingTagUsesDefault() {
    TestEasyNPCData source = new TestEasyNPCData();
    TagValueOutput valueOutput = TagValueOutput.createWithoutContext(ProblemReporter.DISCARDING);

    source.addAdditionalRenderData(valueOutput);

    CompoundTag savedTag = valueOutput.buildResult();
    assertFalse(savedTag.contains(RenderDataCapable.DATA_RENDER_DATA_TAG));

    TestEasyNPCData target = new TestEasyNPCData();
    ValueInput valueInput =
        TagValueInput.create(ProblemReporter.DISCARDING, RegistryAccess.EMPTY, savedTag);
    target.readAdditionalRenderData(valueInput);

    assertEquals(new RenderDataEntry(), target.getRenderDataEntry());
  }

  @Test
  void testModelData_keepsDefaultPoseButSkipsEmptyChildren() {
    TestEasyNPCData source = new TestEasyNPCData();
    source.setDefaultPose(Pose.CROUCHING);
    source.setModelPartPosition(ModelPartType.HEAD, new CustomPosition(0, 0, 0));
    source.setModelPartScale(ModelPartType.RIGHT_ARM, new CustomScale(1, 1, 1));
    source.setModelPartVisibility(ModelPartType.HELMET, true);
    TagValueOutput valueOutput = TagValueOutput.createWithoutContext(ProblemReporter.DISCARDING);

    source.addAdditionalModelData(valueOutput);

    CompoundTag savedTag = valueOutput.buildResult();
    assertTrue(savedTag.contains(ModelDataCapable.EASY_NPC_DATA_MODEL_DATA_TAG));
    CompoundTag modelDataTag =
        savedTag
            .getCompound(ModelDataCapable.EASY_NPC_DATA_MODEL_DATA_TAG)
            .orElse(new CompoundTag());
    assertEquals(
        Pose.CROUCHING.name(),
        modelDataTag.getString(ModelDataCapable.EASY_NPC_DATA_MODEL_DEFAULT_POSE_TAG).orElse(""));
    assertFalse(modelDataTag.contains(ModelPositionDataCapable.EASY_NPC_DATA_MODEL_POSITION_TAG));
    assertFalse(modelDataTag.contains(ModelScaleDataCapable.EASY_NPC_DATA_MODEL_SCALE_TAG));
    assertFalse(modelDataTag.contains(ModelVisibilityDataCapable.EASY_NPC_DATA_MODEL_VISIBLE_TAG));

    TestEasyNPCData target = new TestEasyNPCData();
    ValueInput valueInput =
        TagValueInput.create(ProblemReporter.DISCARDING, RegistryAccess.EMPTY, savedTag);
    target.readAdditionalModelData(valueInput);

    assertEquals(Pose.CROUCHING, target.getDefaultPose());
  }

  @Test
  void testActionData_emptyStateIsSkipped() {
    TestEasyNPCData source = new TestEasyNPCData();
    TagValueOutput valueOutput = TagValueOutput.createWithoutContext(ProblemReporter.DISCARDING);

    source.addAdditionalActionData(valueOutput);

    CompoundTag savedTag = valueOutput.buildResult();
    assertFalse(savedTag.contains(ActionEventDataCapable.DATA_ACTION_DATA_TAG));
  }

  @Test
  void testActionData_defaultInteractionActionsRemainPersisted() {
    TestEasyNPCData source = new TestEasyNPCData();
    source.registerDefaultActionInteractionEvents();
    TagValueOutput valueOutput = TagValueOutput.createWithoutContext(ProblemReporter.DISCARDING);

    source.addAdditionalActionData(valueOutput);

    CompoundTag savedTag = valueOutput.buildResult();
    assertTrue(savedTag.contains(ActionEventDataCapable.DATA_ACTION_DATA_TAG));

    TestEasyNPCData target = new TestEasyNPCData();
    ValueInput valueInput =
        TagValueInput.create(ProblemReporter.DISCARDING, RegistryAccess.EMPTY, savedTag);
    target.readAdditionalActionData(valueInput);

    assertTrue(target.hasActionEvent(ActionEventType.ON_INTERACTION));
  }

  @Test
  void testActionData_nonDefaultPermissionRemainsPersisted() {
    TestEasyNPCData source = new TestEasyNPCData();
    source.setActionPermissionLevel(2);
    TagValueOutput valueOutput = TagValueOutput.createWithoutContext(ProblemReporter.DISCARDING);

    source.addAdditionalActionData(valueOutput);

    CompoundTag savedTag = valueOutput.buildResult();
    assertTrue(savedTag.contains(ActionEventDataCapable.DATA_ACTION_DATA_TAG));
    assertEquals(
        2,
        savedTag
            .getCompound(ActionEventDataCapable.DATA_ACTION_DATA_TAG)
            .flatMap(tag -> tag.getInt(ActionEventDataCapable.DATA_ACTION_PERMISSION_LEVEL_TAG))
            .orElse(0));

    TestEasyNPCData target = new TestEasyNPCData();
    ValueInput valueInput =
        TagValueInput.create(ProblemReporter.DISCARDING, RegistryAccess.EMPTY, savedTag);
    target.readAdditionalActionData(valueInput);

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
      this.defineSynchedNavigationData(null);
      this.defineSynchedRenderData(null);
      this.defineSynchedModelData(null);
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
    public <T> void defineSynchedEntityData(
        SynchedEntityData.Builder builder, SynchedDataIndex synchedDataIndex, T defaultData) {
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
