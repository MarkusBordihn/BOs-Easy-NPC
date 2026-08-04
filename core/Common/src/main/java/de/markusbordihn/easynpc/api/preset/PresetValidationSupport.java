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

import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import java.util.List;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.resources.ResourceLocation;

final class PresetValidationSupport {

  private PresetValidationSupport() {}

  static void validateConditionList(
      ListTag conditionEntries,
      String path,
      PresetValidationContext context,
      List<PresetValidationIssue> issues) {
    for (int index = 0; index < conditionEntries.size(); index++) {
      validateConditionEntry(
          conditionEntries.getCompound(index), path + "[" + index + "]", context, issues);
    }
  }

  private static void validateConditionEntry(
      CompoundTag conditionEntry,
      String path,
      PresetValidationContext context,
      List<PresetValidationIssue> issues) {
    String conditionTypeName = conditionEntry.getString(ConditionDataEntry.DATA_TYPE_TAG);
    ConditionType conditionType = ConditionType.get(conditionTypeName);

    if (conditionType == ConditionType.NONE) {
      issues.add(
          PresetValidationIssue.error(
              PresetValidationRule.UNKNOWN_CONDITION_TYPE,
              path,
              "The condition type '" + conditionTypeName + "' is not known"));
      return;
    }

    if (conditionType != ConditionType.CUSTOM) {
      return;
    }

    String customConditionId =
        conditionEntry.getString(ConditionDataEntry.DATA_CUSTOM_CONDITION_ID_TAG);
    ResourceLocation conditionId = ResourceLocation.tryParse(customConditionId);
    if (conditionId == null) {
      issues.add(
          PresetValidationIssue.error(
              PresetValidationRule.MALFORMED_CUSTOM_CONDITION_ID,
              path,
              "The custom condition id '" + customConditionId + "' is not a valid identifier"));
      return;
    }

    if (!context.knownCustomConditions().test(conditionId)) {
      issues.add(
          PresetValidationIssue.warning(
              PresetValidationRule.UNKNOWN_CUSTOM_CONDITION_ID,
              path,
              "The custom condition '"
                  + conditionId
                  + "' is not registered, so the condition is never met"));
    }
  }

  static void validateText(String text, String path, List<PresetValidationIssue> issues) {
    if (text == null || text.isEmpty()) {
      return;
    }

    for (int index = 0; index < text.length(); index++) {
      char character = text.charAt(index);
      if (Character.isISOControl(character) && character != '\n' && character != '\t') {
        issues.add(
            PresetValidationIssue.error(
                PresetValidationRule.CONTROL_CHARACTER_IN_TEXT,
                path,
                "The text contains the control character " + (int) character));
        return;
      }
    }
  }

  static String childPath(String path, String key) {
    return path.isEmpty() ? key : path + "/" + key;
  }
}
