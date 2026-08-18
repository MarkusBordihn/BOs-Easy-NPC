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

package de.markusbordihn.easynpc.data.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.display.DisplayAttributeDataSet;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.utils.ValueUtils;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import net.minecraft.SharedConstants;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;
import net.minecraft.nbt.TagParser;
import net.minecraft.server.Bootstrap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class GhostPatrolPresetTest {

  private static final Path GHOST_PATROL_PRESET =
      Path.of(
          "src",
          "main",
          "resources",
          "data",
          "easy_npc",
          "default_preset",
          "humanoid",
          "lantern_wraith.npc.snbt");

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private CompoundTag loadEntityData() throws Exception {
    CompoundTag preset =
        TagParser.parseTag(Files.readString(GHOST_PATROL_PRESET, StandardCharsets.UTF_8));
    return preset.getCompound(PresetData.DATA_TAG);
  }

  private List<ActionDataEntry> loadPatrolActions() throws Exception {
    DialogDataSet dialogDataSet =
        new DialogDataSet(this.loadEntityData().getCompound("DialogData"));
    DialogDataEntry dialogDataEntry = dialogDataSet.getDialog("default");
    for (DialogButtonEntry dialogButtonEntry : dialogDataEntry.getDialogButtons()) {
      if ("button_patrol".equals(dialogButtonEntry.label())) {
        return dialogButtonEntry.actionDataSet().getOrderedEntries();
      }
    }

    return List.of();
  }

  @Test
  @DisplayName("The ghost patrol preset is shipped and readable")
  void testGhostPatrolPresetIsShipped() {
    assertTrue(
        Files.isRegularFile(GHOST_PATROL_PRESET),
        "The ghost patrol preset " + GHOST_PATROL_PRESET.toAbsolutePath() + " must exist");
  }

  @Test
  @DisplayName("The ghost returns home with the configured movement speed")
  void testGhostReturnsHomeWithConfiguredMovementSpeed() throws Exception {
    CompoundTag entityData = this.loadEntityData();
    ObjectiveDataSet objectiveDataSet =
        new ObjectiveDataSet(entityData.getCompound("ObjectiveData"));
    assertTrue(objectiveDataSet.hasObjective(ObjectiveType.MOVE_BACK_TO_HOME));

    ListTag attributes = entityData.getList("Attributes", Tag.TAG_COMPOUND);
    boolean hasMovementSpeed = false;
    for (int i = 0; i < attributes.size(); i++) {
      CompoundTag attribute = attributes.getCompound(i);
      if ("minecraft:generic.movement_speed".equals(attribute.getString("Name"))) {
        assertEquals(0.3D, attribute.getDouble("Base"));
        hasMovementSpeed = true;
      }
    }
    assertTrue(hasMovementSpeed, "The preset must define its movement speed");
  }

  @Test
  @DisplayName("The patrol button walks a closed square with four blocking move actions")
  void testPatrolWalksAClosedSquare() throws Exception {
    List<ActionDataEntry> actionDataEntries = this.loadPatrolActions();
    assertEquals(ActionDataType.CLOSE_DIALOG, actionDataEntries.get(0).actionDataType());

    List<BlockPos> offsets = new ArrayList<>();
    for (ActionDataEntry actionDataEntry : actionDataEntries) {
      if (actionDataEntry.actionDataType() != ActionDataType.MOVE_TO_AND_WAIT) {
        continue;
      }
      assertEquals(MoveTargetType.RELATIVE, actionDataEntry.moveActionData().targetType());
      assertEquals(
          MoveActionData.DEFAULT_SPEED_MODIFIER,
          actionDataEntry.moveActionData().speedModifier());
      assertTrue(
          actionDataEntry.isValidAndNotEmpty(),
          "Move action " + actionDataEntry + " must resolve a target");
      offsets.add(actionDataEntry.blockPos());
    }

    assertEquals(4, offsets.size());
    BlockPos closedSquare = BlockPos.ZERO;
    for (BlockPos offset : offsets) {
      assertNotEquals(BlockPos.ZERO, offset);
      closedSquare = closedSquare.offset(offset);
    }
    assertEquals(BlockPos.ZERO, closedSquare);
  }

  @Test
  @DisplayName("The patrol fades the ghost out and back to its preset opacity")
  void testPatrolFadesBackToThePresetOpacity() throws Exception {
    int presetOpacity =
        new DisplayAttributeDataSet(
                this.loadEntityData()
                    .getList(
                        DisplayAttributeDataCapable.DATA_DISPLAY_ATTRIBUTE_TAG, Tag.TAG_COMPOUND))
            .getAttribute(DisplayAttributeType.OPACITY)
            .intValue();
    assertEquals(40, presetOpacity);

    List<Integer> opacityValues = new ArrayList<>();
    for (ActionDataEntry actionDataEntry : this.loadPatrolActions()) {
      if (actionDataEntry.actionDataType() != ActionDataType.SET_OPACITY) {
        continue;
      }
      assertTrue(
          ValueUtils.isNumericValue(
              actionDataEntry.command(),
              DisplayAttributeType.MIN_OPACITY,
              DisplayAttributeType.MAX_OPACITY),
          "Opacity " + actionDataEntry.command() + " must be within the allowed range");
      opacityValues.add(Integer.parseInt(actionDataEntry.command()));
    }

    assertEquals(2, opacityValues.size());
    assertTrue(opacityValues.get(0) < presetOpacity);
    assertEquals(presetOpacity, opacityValues.get(1));
  }
}
