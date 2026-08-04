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

package de.markusbordihn.easynpc.api.preset;

import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import java.util.List;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;

final class PresetObjectiveValidator {

  private PresetObjectiveValidator() {}

  static void validateObjectiveData(
      CompoundTag entityData, String path, List<PresetValidationIssue> issues) {
    if (!entityData.contains(ObjectiveDataCapable.DATA_OBJECTIVE_DATA_TAG)) {
      return;
    }

    ListTag objectiveEntries =
        entityData
            .getCompound(ObjectiveDataCapable.DATA_OBJECTIVE_DATA_TAG)
            .getList(ObjectiveDataSet.DATA_OBJECTIVE_DATA_SET_TAG, Tag.TAG_COMPOUND);
    String objectivePath =
        PresetValidationSupport.childPath(path, ObjectiveDataCapable.DATA_OBJECTIVE_DATA_TAG)
            + "/"
            + ObjectiveDataSet.DATA_OBJECTIVE_DATA_SET_TAG;

    for (int index = 0; index < objectiveEntries.size(); index++) {
      CompoundTag objectiveEntry = objectiveEntries.getCompound(index);
      String entryPath = objectivePath + "[" + index + "]";
      String objectiveTypeName = objectiveEntry.getString(ObjectiveDataEntry.DATA_TYPE_TAG);

      if (ObjectiveType.get(objectiveTypeName) == null) {
        issues.add(
            PresetValidationIssue.warning(
                PresetValidationRule.UNKNOWN_OBJECTIVE_TYPE,
                entryPath,
                "The objective type '" + objectiveTypeName + "' is not known here"));
        continue;
      }

      validateObjectiveRange(objectiveEntry, entryPath, issues);
    }
  }

  private static void validateObjectiveRange(
      CompoundTag objectiveEntry, String path, List<PresetValidationIssue> issues) {
    if (objectiveEntry.contains(ObjectiveDataEntry.DATA_PROBABILITY_TAG)) {
      float probability = objectiveEntry.getFloat(ObjectiveDataEntry.DATA_PROBABILITY_TAG);
      if (probability < 0.0F || probability > 1.0F) {
        issues.add(
            PresetValidationIssue.warning(
                PresetValidationRule.OBJECTIVE_VALUE_OUT_OF_RANGE,
                path,
                "The probability " + probability + " is outside of 0.0 to 1.0"));
      }
    }

    for (String distanceTag :
        List.of(
            ObjectiveDataEntry.DATA_STOP_DISTANCE_TAG,
            ObjectiveDataEntry.DATA_START_DISTANCE_TAG,
            ObjectiveDataEntry.DATA_LOOK_DISTANCE_TAG)) {
      if (objectiveEntry.contains(distanceTag) && objectiveEntry.getFloat(distanceTag) < 0.0F) {
        issues.add(
            PresetValidationIssue.warning(
                PresetValidationRule.OBJECTIVE_VALUE_OUT_OF_RANGE,
                path,
                "The value of " + distanceTag + " must not be negative"));
      }
    }
  }
}
