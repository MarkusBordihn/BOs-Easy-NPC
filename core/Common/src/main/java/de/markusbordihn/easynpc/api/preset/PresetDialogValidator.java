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

import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogDataCapable;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;

final class PresetDialogValidator {

  private PresetDialogValidator() {}

  static void validateDialogData(
      CompoundTag entityData,
      String path,
      PresetValidationContext context,
      List<PresetValidationIssue> issues) {
    if (!entityData.contains(DialogDataCapable.DATA_DIALOG_DATA_TAG)) {
      return;
    }

    String dialogPath =
        PresetValidationSupport.childPath(path, DialogDataCapable.DATA_DIALOG_DATA_TAG);
    ListTag dialogEntries =
        entityData
            .getCompoundOrEmpty(DialogDataCapable.DATA_DIALOG_DATA_TAG)
            .getListOrEmpty(DialogDataSet.DATA_DIALOG_DATA_SET_TAG);
    Set<String> dialogLabels = new HashSet<>();

    for (int index = 0; index < dialogEntries.size(); index++) {
      CompoundTag dialogEntry = dialogEntries.getCompoundOrEmpty(index);
      String entryPath =
          dialogPath + "/" + DialogDataSet.DATA_DIALOG_DATA_SET_TAG + "[" + index + "]";
      String dialogLabel = dialogEntry.getStringOr(DialogDataEntry.DATA_LABEL_TAG, "");

      if (!dialogLabel.isEmpty() && !dialogLabels.add(dialogLabel)) {
        issues.add(
            PresetValidationIssue.error(
                PresetValidationRule.DUPLICATE_DIALOG_LABEL,
                entryPath,
                "The dialog label '" + dialogLabel + "' is used more than once"));
      }

      if (dialogLabel.length() > DialogDataEntry.MAX_DIALOG_LABEL_LENGTH) {
        issues.add(
            PresetValidationIssue.error(
                PresetValidationRule.DIALOG_LABEL_TOO_LONG,
                entryPath,
                "The dialog label '"
                    + dialogLabel
                    + "' is longer than "
                    + DialogDataEntry.MAX_DIALOG_LABEL_LENGTH
                    + " characters and is shortened when it is loaded"));
      }

      if (dialogEntry.getListOrEmpty(DialogDataEntry.DATA_TEXTS_TAG).isEmpty()) {
        issues.add(
            PresetValidationIssue.error(
                PresetValidationRule.DIALOG_WITHOUT_TEXT, entryPath, "The dialog has no text"));
      }

      PresetValidationSupport.validateText(dialogLabel, entryPath, issues);
      PresetValidationSupport.validateConditionList(
          dialogEntry.getListOrEmpty(DialogDataEntry.DATA_CONDITIONS_TAG),
          PresetValidationSupport.childPath(entryPath, DialogDataEntry.DATA_CONDITIONS_TAG),
          context,
          issues);
      validateDialogButtons(dialogEntry, entryPath, context, issues);
    }
  }

  private static void validateDialogButtons(
      CompoundTag dialogEntry,
      String path,
      PresetValidationContext context,
      List<PresetValidationIssue> issues) {
    ListTag dialogButtons = dialogEntry.getListOrEmpty(DialogDataEntry.DATA_BUTTONS_TAG);

    for (int index = 0; index < dialogButtons.size(); index++) {
      CompoundTag dialogButton = dialogButtons.getCompoundOrEmpty(index);
      String buttonPath = path + "/" + DialogDataEntry.DATA_BUTTONS_TAG + "[" + index + "]";
      PresetValidationSupport.validateConditionList(
          dialogButton.getListOrEmpty(DialogButtonEntry.DATA_CONDITIONS_TAG),
          PresetValidationSupport.childPath(buttonPath, DialogButtonEntry.DATA_CONDITIONS_TAG),
          context,
          issues);

      ListTag buttonActions =
          dialogButton.getListOrEmpty(DialogButtonEntry.DATA_ACTIONS_TAG);
      for (int actionIndex = 0; actionIndex < buttonActions.size(); actionIndex++) {
        PresetActionValidator.validateActionEntry(
            buttonActions.getCompoundOrEmpty(actionIndex),
            buttonPath + "/" + DialogButtonEntry.DATA_ACTIONS_TAG + "[" + actionIndex + "]",
            context,
            issues);
      }
    }
  }
}
