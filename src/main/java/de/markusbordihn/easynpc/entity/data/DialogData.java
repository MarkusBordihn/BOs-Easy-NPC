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

import de.markusbordihn.easynpc.dialog.DialogType;
import de.markusbordihn.easynpc.entity.EasyNPCEntityData;

public interface DialogData extends DataInterface {

  // Synced entity data
  public static final EntityDataAccessor<DialogType> DATA_DIALOG_TYPE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, DataSerializers.DIALOG_TYPE);
  public static final EntityDataAccessor<String> DATA_DIALOG =
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
  public static final String DATA_DIALOG_NO_BUTTON_TAG = "DialogNoButton";
  public static final String DATA_DIALOG_NO_TAG = "DialogNo";
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

  default boolean hasDialog() {
    return !getEntityData(DATA_DIALOG).isEmpty() && getDialogType() != DialogType.NONE;
  }

  default String getDialog() {
    return getEntityData(DATA_DIALOG);
  }

  default void setDialog(String dialog) {
    setEntityData(DATA_DIALOG, dialog);
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
    defineEntityData(DATA_DIALOG, "");
    defineEntityData(DATA_DIALOG_TYPE, DialogType.NONE);
    defineEntityData(DATA_DIALOG_NO, "");
    defineEntityData(DATA_DIALOG_NO_BUTTON, "No");
    defineEntityData(DATA_DIALOG_YES, "");
    defineEntityData(DATA_DIALOG_YES_BUTTON, "Yes");
  }

  default void addAdditionalDialogData(CompoundTag compoundTag) {
    if (this.getDialog() != null) {
      compoundTag.putString(DATA_DIALOG_TAG, this.getDialog());
    }
    if (this.getDialogType() != null) {
      compoundTag.putString(DATA_DIALOG_TYPE_TAG, this.getDialogType().name());
    }
    if (this.getNoDialog() != null) {
      compoundTag.putString(DATA_DIALOG_NO_TAG, this.getNoDialog());
    }
    if (this.getNoDialogButton() != null) {
      compoundTag.putString(DATA_DIALOG_NO_BUTTON_TAG, this.getNoDialogButton());
    }
    if (this.getYesDialog() != null) {
      compoundTag.putString(DATA_DIALOG_NO_TAG, this.getYesDialog());
    }
    if (this.getYesDialogButton() != null) {
      compoundTag.putString(DATA_DIALOG_YES_BUTTON_TAG, this.getYesDialogButton());
    }
  }

  default void readAdditionalDialogData(CompoundTag compoundTag) {
    if (compoundTag.contains(DATA_DIALOG_TAG)) {
      String dialog = compoundTag.getString(DATA_DIALOG_TAG);
      if (dialog != null) {
        this.setDialog(dialog);
      }
    }
    if (compoundTag.contains(DATA_DIALOG_TYPE_TAG)) {
      String dialogType = compoundTag.getString(DATA_DIALOG_TYPE_TAG);
      if (dialogType != null && !dialogType.isEmpty()) {
        this.setDialogType(DialogType.get(dialogType));
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
