/**
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

package de.markusbordihn.easynpc.entity.data;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;

import de.markusbordihn.easynpc.data.dialog.DialogType;
import de.markusbordihn.easynpc.entity.EasyNPCEntityData;

public interface EntityDialogData extends EntityDataInterface {

  // Synced entity data
  public static final EntityDataAccessor<DialogType> DATA_DIALOG_TYPE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.DIALOG_TYPE);
  public static final EntityDataAccessor<String> DATA_DIALOG_SIMPLE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  public static final EntityDataAccessor<String> DATA_DIALOG_NO =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  public static final EntityDataAccessor<String> DATA_DIALOG_NO_BUTTON =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  public static final EntityDataAccessor<String> DATA_DIALOG_YES =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  public static final EntityDataAccessor<String> DATA_DIALOG_YES_BUTTON =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);

  // CompoundTags
  public static final String DATA_DIALOG_DATA_TAG = "DialogData";
  public static final String DATA_DIALOG_NO_BUTTON_TAG = "DialogNoButton";
  public static final String DATA_DIALOG_NO_TAG = "DialogNo";
  public static final String DATA_DIALOG_SIMPLE_TAG = "DialogSimple";
  public static final String DATA_DIALOG_TAG = "Dialog";
  public static final String DATA_DIALOG_TYPE_TAG = "DialogType";
  public static final String DATA_DIALOG_YES_BUTTON_TAG = "DialogYesButton";
  public static final String DATA_DIALOG_YES_TAG = "DialogYes";

  default DialogType getDialogType() {
    return getEntityData(DATA_DIALOG_TYPE);
  }

  default void setDialogType(DialogType dialogType) {
    setEntityData(DATA_DIALOG_TYPE, dialogType);
  }

  default boolean hasSimpleDialog() {
    return !getEntityData(DATA_DIALOG_SIMPLE).isEmpty() && getDialogType() != DialogType.NONE;
  }

  default String getSimpleDialog() {
    return getEntityData(DATA_DIALOG_SIMPLE);
  }

  default void setSimpleDialog(String dialog) {
    setEntityData(DATA_DIALOG_SIMPLE, dialog);
  }

  default String getNoDialog() {
    return getEntityData(DATA_DIALOG_NO);
  }

  default void setNoDialog(String dialog) {
    setEntityData(DATA_DIALOG_NO, dialog);
  }

  default String getNoDialogButton() {
    return getEntityData(DATA_DIALOG_NO_BUTTON);
  }

  default void setNoDialogButton(String dialogButton) {
    setEntityData(DATA_DIALOG_NO_BUTTON, dialogButton);
  }

  default String getYesDialog() {
    return getEntityData(DATA_DIALOG_YES);
  }

  default void setYesDialog(String dialog) {
    setEntityData(DATA_DIALOG_YES, dialog);
  }

  default String getYesDialogButton() {
    return getEntityData(DATA_DIALOG_YES_BUTTON);
  }

  default void setYesDialogButton(String dialogButton) {
    setEntityData(DATA_DIALOG_YES_BUTTON, dialogButton);
  }

  default void defineSynchedDialogData() {
    defineEntityData(DATA_DIALOG_SIMPLE, "");
    defineEntityData(DATA_DIALOG_TYPE, DialogType.NONE);
    defineEntityData(DATA_DIALOG_NO, "");
    defineEntityData(DATA_DIALOG_NO_BUTTON, "No");
    defineEntityData(DATA_DIALOG_YES, "");
    defineEntityData(DATA_DIALOG_YES_BUTTON, "Yes");
  }

  default void addAdditionalDialogData(CompoundTag compoundTag) {
    CompoundTag dialogTag = new CompoundTag();
    if (this.getSimpleDialog() != null) {
      dialogTag.putString(DATA_DIALOG_SIMPLE_TAG, this.getSimpleDialog());
    }
    if (this.getDialogType() != null) {
      dialogTag.putString(DATA_DIALOG_TYPE_TAG, this.getDialogType().name());
    }
    if (this.getNoDialog() != null) {
      dialogTag.putString(DATA_DIALOG_NO_TAG, this.getNoDialog());
    }
    if (this.getNoDialogButton() != null) {
      dialogTag.putString(DATA_DIALOG_NO_BUTTON_TAG, this.getNoDialogButton());
    }
    if (this.getYesDialog() != null) {
      dialogTag.putString(DATA_DIALOG_YES_TAG, this.getYesDialog());
    }
    if (this.getYesDialogButton() != null) {
      dialogTag.putString(DATA_DIALOG_YES_BUTTON_TAG, this.getYesDialogButton());
    }
    compoundTag.put(DATA_DIALOG_DATA_TAG, dialogTag);
  }

  default void readAdditionalDialogData(CompoundTag compoundTag) {

    // Legacy dialog data support
    readAdditionalLegacyDialogData(compoundTag);

    // Early exit if no dialog data is available.
    if (!compoundTag.contains(DATA_DIALOG_DATA_TAG)) {
      return;
    }

    // Read dialog data
    CompoundTag dialogTag = compoundTag.getCompound(DATA_DIALOG_DATA_TAG);
    if (dialogTag.contains(DATA_DIALOG_SIMPLE_TAG)) {
      String dialog = dialogTag.getString(DATA_DIALOG_SIMPLE_TAG);
      if (dialog != null) {
        this.setSimpleDialog(dialog);
      }
    }
    if (dialogTag.contains(DATA_DIALOG_TYPE_TAG)) {
      String dialogType = dialogTag.getString(DATA_DIALOG_TYPE_TAG);
      if (dialogType != null && !dialogType.isEmpty()) {
        this.setDialogType(DialogType.get(dialogType));
      }
    }
    if (dialogTag.contains(DATA_DIALOG_NO_TAG)) {
      String dialog = dialogTag.getString(DATA_DIALOG_NO_TAG);
      if (dialog != null) {
        this.setNoDialog(dialog);
      }
    }
    if (dialogTag.contains(DATA_DIALOG_NO_BUTTON_TAG)) {
      String dialogButton = dialogTag.getString(DATA_DIALOG_NO_BUTTON_TAG);
      if (dialogButton != null) {
        this.setNoDialogButton(dialogButton);
      }
    }
    if (dialogTag.contains(DATA_DIALOG_YES_TAG)) {
      String dialog = dialogTag.getString(DATA_DIALOG_YES_TAG);
      if (dialog != null) {
        this.setYesDialog(dialog);
      }
    }
    if (dialogTag.contains(DATA_DIALOG_YES_BUTTON_TAG)) {
      String dialogButton = dialogTag.getString(DATA_DIALOG_YES_BUTTON_TAG);
      if (dialogButton != null) {
        this.setYesDialogButton(dialogButton);
      }
    }

  }

  default void readAdditionalLegacyDialogData(CompoundTag compoundTag) {
    if (compoundTag.contains(DATA_DIALOG_TYPE_TAG)) {
      log.info("Converting legacy dialog data to new format for {}", this);
      String dialogType = compoundTag.getString(DATA_DIALOG_TYPE_TAG);
      if (dialogType != null && !dialogType.isEmpty()) {
        this.setDialogType(DialogType.get(dialogType));
      }
      if (compoundTag.contains(DATA_DIALOG_TAG)) {
        String dialog = compoundTag.getString(DATA_DIALOG_TAG);
        if (dialog != null) {
          this.setSimpleDialog(dialog);
        }
      }
      if (compoundTag.contains(DATA_DIALOG_NO_TAG)) {
        String dialog = compoundTag.getString(DATA_DIALOG_NO_TAG);
        if (dialog != null) {
          this.setNoDialog(dialog);
        }
      }
      if (compoundTag.contains(DATA_DIALOG_NO_BUTTON_TAG)) {
        String dialogButton = compoundTag.getString(DATA_DIALOG_NO_BUTTON_TAG);
        if (dialogButton != null) {
          this.setNoDialogButton(dialogButton);
        }
      }
      if (compoundTag.contains(DATA_DIALOG_YES_TAG)) {
        String dialog = compoundTag.getString(DATA_DIALOG_YES_TAG);
        if (dialog != null) {
          this.setYesDialog(dialog);
        }
      }
      if (compoundTag.contains(DATA_DIALOG_YES_BUTTON_TAG)) {
        String dialogButton = compoundTag.getString(DATA_DIALOG_YES_BUTTON_TAG);
        if (dialogButton != null) {
          this.setYesDialogButton(dialogButton);
        }
      }
    }
  }

}
