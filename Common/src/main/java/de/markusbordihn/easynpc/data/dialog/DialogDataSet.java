/*
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

package de.markusbordihn.easynpc.data.dialog;

import de.markusbordihn.easynpc.Constants;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DialogDataSet {

  // Dialog Data Tags
  public static final String DATA_DIALOG_DATA_SET_TAG = "DialogDataSet";
  public static final String DATA_DIALOG_DEFAULT_TAG = "Default";
  public static final String DATA_TYPE_TAG = "Type";
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  // Cache
  private final HashMap<String, DialogDataEntry> dialogByLabelMap = new HashMap<>();
  private final HashMap<UUID, DialogDataEntry> dialogByIdMap = new HashMap<>();
  private String defaultDialogLabel = "default";
  private DialogType dialogType = DialogType.STANDARD;

  public DialogDataSet() {}

  public DialogDataSet(DialogType dialogType) {
    this.dialogType = dialogType;
  }

  public DialogDataSet(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public void addDefaultDialog(DialogDataEntry dialogData) {
    if (this.addDialog(dialogData)) {
      this.setDefaultDialog(dialogData);
    }
  }

  public void setDialog(UUID dialogId, DialogDataEntry dialogData) {
    if (dialogData == null) {
      log.error("Dialog data is null, please check your dialog data!");
      return;
    }
    if (this.hasDialog(dialogId)) {
      removeDialog(dialogId);
    }
    this.addDialog(dialogData);
  }

  public boolean addDialog(DialogDataEntry dialogData) {
    // Pre-check dialog data, before adding it to the dialog set.
    if (dialogData == null) {
      log.error("Dialog data is null, please check your dialog data!");
      return false;
    }
    if (dialogData.getId() == null) {
      log.error("Dialog id is null, please check your dialog data!");
      return false;
    }
    if (dialogData.getLabel() == null) {
      log.error("Dialog label is null, please check your dialog data!");
      return false;
    }
    if (dialogData.getText() == null || dialogData.getText().isEmpty()) {
      log.error("Dialog text is null or empty, please check your dialog data!");
      return false;
    }

    String dialogLabel = dialogData.getLabel();
    UUID dialogId = dialogData.getId();

    // Warn about duplicated dialog ids
    DialogDataEntry existingDialogData = this.dialogByIdMap.getOrDefault(dialogId, null);
    if (existingDialogData != null && !existingDialogData.equals(dialogData)) {
      log.warn(
          "Duplicated dialog with id {} found, will overwrite existing dialog {} with {}!",
          dialogId,
          dialogData,
          existingDialogData);
    }

    this.dialogByLabelMap.put(dialogLabel, dialogData);
    this.dialogByIdMap.put(dialogId, dialogData);
    return true;
  }

  public boolean removeDialog(UUID dialogId) {
    DialogDataEntry dialogData = this.dialogByIdMap.getOrDefault(dialogId, null);
    if (dialogData != null) {
      DialogDataEntry formerDialogData = this.dialogByIdMap.remove(dialogData.getId());
      if (formerDialogData != null) {
        this.dialogByLabelMap.remove(formerDialogData.getLabel());
      }
      return true;
    }
    return false;
  }

  public boolean removeDialogButton(UUID dialogId, UUID dialogButtonId) {
    DialogDataEntry dialogData = this.dialogByIdMap.getOrDefault(dialogId, null);
    if (dialogData != null) {
      return dialogData.removeDialogButton(dialogButtonId);
    }
    return false;
  }

  public List<DialogDataEntry> getDialogsByLabel() {
    return this.dialogByLabelMap.values().stream()
        .sorted(Comparator.comparing(DialogDataEntry::getLabel))
        .toList();
  }

  public DialogDataEntry getDialog(String label) {
    return this.dialogByLabelMap.getOrDefault(label, null);
  }

  public DialogDataEntry getDialog(UUID id) {
    return this.dialogByIdMap.getOrDefault(id, null);
  }

  public UUID getDialogId(String dialogLabel) {
    DialogDataEntry dialogData = this.dialogByLabelMap.getOrDefault(dialogLabel, null);
    if (dialogData != null) {
      return dialogData.getId();
    }
    return null;
  }

  public boolean hasDialog() {
    return !this.dialogByLabelMap.isEmpty();
  }

  public boolean hasDialog(String label) {
    return this.dialogByLabelMap.containsKey(label);
  }

  public boolean hasDialog(UUID id) {
    return this.dialogByIdMap.containsKey(id);
  }

  public boolean hasDialogButton(String dialogLabel, UUID dialogButtonId) {
    return this.dialogByLabelMap.containsKey(dialogLabel)
        && this.dialogByLabelMap.get(dialogLabel).hasDialogButton(dialogButtonId);
  }

  public boolean hasDialogButton(UUID dialogId, UUID dialogButtonId) {
    return this.dialogByIdMap.containsKey(dialogId)
        && this.dialogByIdMap.get(dialogId).hasDialogButton(dialogButtonId);
  }

  public DialogButtonEntry getDialogButton(String dialogLabel, UUID dialogButtonId) {
    DialogDataEntry dialogData = this.dialogByLabelMap.getOrDefault(dialogLabel, null);
    if (dialogData != null) {
      return dialogData.getDialogButton(dialogButtonId);
    }
    return null;
  }

  public DialogButtonEntry getDialogButton(UUID dialogId, UUID dialogButtonId) {
    DialogDataEntry dialogData = this.dialogByIdMap.getOrDefault(dialogId, null);
    if (dialogData != null) {
      return dialogData.getDialogButton(dialogButtonId);
    }
    return null;
  }

  public DialogDataEntry getDefaultDialog() {
    return this.dialogByLabelMap.getOrDefault(this.getDefaultDialogLabel(), null);
  }

  public void setDefaultDialog(DialogDataEntry dialogData) {
    if (dialogData != null
        && dialogData.getId() != null
        && dialogData.getLabel() != null
        && !dialogData.getText().isEmpty()) {
      this.defaultDialogLabel = dialogData.getLabel();
    }
  }

  public String getDefaultDialogLabel() {
    // Return cached default dialog label if still available.
    if (this.hasDialog(this.defaultDialogLabel)) {
      return this.defaultDialogLabel;
    } else {
      this.defaultDialogLabel = null;
    }

    // Try common default dialog labels.
    if (this.hasDialog("default")) {
      this.defaultDialogLabel = "default";
    } else if (this.hasDialog("start")) {
      this.defaultDialogLabel = "start";
    } else if (this.hasDialog("main")) {
      this.defaultDialogLabel = "main";
    }
    if (this.defaultDialogLabel != null) {
      return this.defaultDialogLabel;
    }

    // Iterate over all dialogs and return the first valid dialog.
    for (DialogDataEntry dialogData : this.dialogByLabelMap.values()) {
      if (dialogData != null
          && dialogData.getId() != null
          && dialogData.getLabel() != null
          && !dialogData.getLabel().isEmpty()
          && !dialogData.getText().isEmpty()) {
        this.defaultDialogLabel = dialogData.getLabel();
        return this.defaultDialogLabel;
      }
    }

    return null;
  }

  public UUID getDefaultDialogId() {
    String currentDefaultDialogLabel = this.getDefaultDialogLabel();
    if (currentDefaultDialogLabel == null) {
      return null;
    }
    DialogDataEntry dialogData =
        this.dialogByLabelMap.getOrDefault(currentDefaultDialogLabel, null);
    if (dialogData != null) {
      return dialogData.getId();
    }
    return null;
  }

  public DialogType getType() {
    return this.dialogType;
  }

  public void load(CompoundTag compoundTag) {
    if (compoundTag == null || !compoundTag.contains(DATA_DIALOG_DATA_SET_TAG)) {
      return;
    }

    // Load dialog type
    if (compoundTag.contains(DATA_TYPE_TAG)) {
      this.dialogType = DialogType.valueOf(compoundTag.getString(DATA_TYPE_TAG));
    }

    // Load dialog data
    this.dialogByLabelMap.clear();
    this.dialogByIdMap.clear();
    ListTag dialogListTag = compoundTag.getList(DATA_DIALOG_DATA_SET_TAG, 10);
    for (int i = 0; i < dialogListTag.size(); ++i) {
      CompoundTag dialogCompoundTag = dialogListTag.getCompound(i);
      DialogDataEntry dialogData = new DialogDataEntry(dialogCompoundTag);
      this.addDialog(dialogData);
    }

    // Load default dialog index
    if (compoundTag.contains(DATA_DIALOG_DEFAULT_TAG)) {
      String defaultDialogLabelData = compoundTag.getString(DATA_DIALOG_DEFAULT_TAG);
      if (!defaultDialogLabelData.isEmpty()
          && this.dialogByLabelMap.containsKey(defaultDialogLabelData)) {
        this.defaultDialogLabel = defaultDialogLabelData;
      } else {
        log.warn(
            "Invalid default dialog index `{}` found, will use {} instead!",
            defaultDialogLabelData,
            this.getDefaultDialogLabel());
      }
    }
  }

  public CompoundTag save(CompoundTag compoundTag) {
    ListTag dialogListTag = new ListTag();
    for (DialogDataEntry dialogData : this.dialogByLabelMap.values()) {
      // Skip empty dialog data
      if (dialogData == null
          || dialogData.getId() == null
          || dialogData.getLabel() == null
          || dialogData.getText().isEmpty()) {
        continue;
      }
      dialogListTag.add(dialogData.createTag());
    }
    compoundTag.put(DATA_DIALOG_DATA_SET_TAG, dialogListTag);

    // Handle dialog type to avoid wrong dialog types after using the dialog editor.
    if ((this.dialogType == DialogType.BASIC && this.dialogByIdMap.size() > 1)
        || (this.dialogType == DialogType.YES_NO && this.dialogByIdMap.size() > 3)) {
      this.dialogType = DialogType.STANDARD;
    } else if (this.dialogByIdMap.isEmpty()) {
      this.dialogType = DialogType.NONE;
    } else if (this.dialogType != DialogType.BASIC
        && this.dialogType != DialogType.YES_NO
        && this.dialogType != DialogType.STANDARD) {
      this.dialogType = DialogType.CUSTOM;
    }
    compoundTag.putString(DATA_TYPE_TAG, this.dialogType.name());

    // Only save default dialog label if there is any.
    if (this.defaultDialogLabel != null
        && !this.defaultDialogLabel.isEmpty()
        && this.hasDialog(this.defaultDialogLabel)) {
      compoundTag.putString(DATA_DIALOG_DEFAULT_TAG, this.defaultDialogLabel);
    }

    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.save(new CompoundTag());
  }

  @Override
  public String toString() {
    return "DialogDataSet [type="
        + this.dialogType
        + ", default="
        + this.defaultDialogLabel
        + ", "
        + this.dialogByLabelMap
        + "]";
  }
}
