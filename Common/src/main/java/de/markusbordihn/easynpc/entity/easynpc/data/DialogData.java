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

package de.markusbordihn.easynpc.entity.easynpc.data;

import de.markusbordihn.easynpc.data.action.ActionData;
import de.markusbordihn.easynpc.data.action.ActionType;
import de.markusbordihn.easynpc.data.custom.CustomDataAccessor;
import de.markusbordihn.easynpc.data.custom.CustomDataIndex;
import de.markusbordihn.easynpc.data.dialog.DialogButtonData;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.dialog.DialogType;
import de.markusbordihn.easynpc.data.dialog.DialogUtils;
import de.markusbordihn.easynpc.data.entity.CustomEntityData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.legacy.LegacyDialogSetData;
import java.util.Set;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.syncher.EntityDataSerializer;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.world.entity.LivingEntity;

public interface DialogData<T extends LivingEntity> extends EasyNPC<T> {

  EntityDataSerializer<DialogDataSet> DIALOG_DATA_SET =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, DialogDataSet value) {
          buffer.writeNbt(value.createTag());
        }

        public DialogDataSet read(FriendlyByteBuf buffer) {
          return new DialogDataSet(buffer.readNbt());
        }

        public DialogDataSet copy(DialogDataSet value) {
          return value;
        }
      };

  CustomDataAccessor<DialogDataSet> CUSTOM_DATA_DIALOG_DATA_SET =
      CustomEntityData.defineId(CustomDataIndex.DIALOG_DATA_SET, DIALOG_DATA_SET);

  String DATA_DIALOG_DATA_TAG = "DialogData";
  String DATA_DIALOG_NO_BUTTON_TAG = "DialogNoButton";
  String DATA_DIALOG_NO_TAG = "DialogNo";
  String DATA_DIALOG_SIMPLE_TAG = "DialogSimple";
  String DATA_DIALOG_TAG = "Dialog";
  String DATA_DIALOG_TYPE_TAG = "DialogType";
  String DATA_DIALOG_YES_BUTTON_TAG = "DialogYesButton";
  String DATA_DIALOG_YES_TAG = "DialogYes";

  public static void registerDialogDataSerializer() {
    EntityDataSerializers.registerSerializer(DIALOG_DATA_SET);
  }

  default DialogDataSet getDialogDataSet() {
    return getEasyNPCCustomData(CUSTOM_DATA_DIALOG_DATA_SET);
  }

  default void setDialogDataSet(DialogDataSet dialogDataSet) {
    setEasyNPCCustomData(CUSTOM_DATA_DIALOG_DATA_SET, dialogDataSet);
  }

  default void clearDialogDataSet() {
    setEasyNPCCustomData(CUSTOM_DATA_DIALOG_DATA_SET, new DialogDataSet());
  }

  default boolean hasDialog() {
    return getDialogDataSet().hasDialog();
  }

  default boolean hasDialog(String dialogLabel) {
    return getDialogDataSet().hasDialog(dialogLabel);
  }

  default boolean hasDialog(UUID dialogId) {
    return getDialogDataSet().hasDialog(dialogId);
  }

  default boolean removeDialog(UUID dialogId) {
    return getDialogDataSet().removeDialog(dialogId);
  }

  default boolean removeDialogButton(UUID dialogId, UUID dialogButtonId) {
    return getDialogDataSet().removeDialogButton(dialogId, dialogButtonId);
  }

  default void setDialog(UUID dialogId, DialogDataEntry dialogData) {
    getDialogDataSet().setDialog(dialogId, dialogData);
  }

  default UUID getDialogId(String dialogLabel) {
    return getDialogDataSet().getDialogId(dialogLabel);
  }

  default boolean hasDialogButton(UUID dialogId, UUID dialogButtonId) {
    return getDialogDataSet().hasDialogButton(dialogId, dialogButtonId);
  }

  default DialogButtonData getDialogButton(UUID dialogId, UUID dialogButtonId) {
    return getDialogDataSet().getDialogButton(dialogId, dialogButtonId);
  }

  default void defineSynchedDialogData() {
  }

  default void defineCustomDialogData() {
    defineEasyNPCCustomData(CUSTOM_DATA_DIALOG_DATA_SET, new DialogDataSet());
  }

  default void addAdditionalDialogData(CompoundTag compoundTag) {
    CompoundTag dialogTag = new CompoundTag();

    // Write dialog data
    getDialogDataSet().save(dialogTag);

    compoundTag.put(DATA_DIALOG_DATA_TAG, dialogTag);
  }

  default void readAdditionalDialogData(CompoundTag compoundTag) {

    // Legacy dialog data support
    if (readAdditionalLegacyDialogData(compoundTag)) {
      return;
    }

    // Early exit if no dialog data is available.
    if (!compoundTag.contains(DATA_DIALOG_DATA_TAG)) {
      return;
    }

    // Read dialog data
    CompoundTag dialogDataTag = compoundTag.getCompound(DATA_DIALOG_DATA_TAG);

    // Read dialog
    if (dialogDataTag.contains(DialogDataSet.DATA_DIALOG_DATA_SET_TAG)) {
      DialogDataSet dialogDataSet = new DialogDataSet(dialogDataTag);
      this.setDialogDataSet(dialogDataSet);
    }
  }

  default boolean readAdditionalLegacyDialogData(CompoundTag compoundTag) {

    DialogDataSet dialogDataSet;

    // Very old legacy data support (just for compatibility with old worlds)
    if (compoundTag.contains(DATA_DIALOG_TYPE_TAG)) {
      log.info("Converting legacy dialog data to new format for {}", this);
      DialogType dialogType = DialogType.get(compoundTag.getString(DATA_DIALOG_TYPE_TAG));

      // Handle basic dialog
      if (dialogType == DialogType.BASIC) {
        dialogDataSet = DialogUtils.getBasicDialog(compoundTag.getString(DATA_DIALOG_TAG));
        this.setDialogDataSet(dialogDataSet);
        return true;
      }

      // Handle yes/no dialog
      if (dialogType != DialogType.YES_NO) {
        return false;
      }

      // Create yes/no dialog data set
      dialogDataSet =
          DialogUtils.getYesNoDialog(
              compoundTag.getString(DATA_DIALOG_TAG),
              compoundTag.getString(DATA_DIALOG_YES_BUTTON_TAG),
              compoundTag.getString(DATA_DIALOG_NO_BUTTON_TAG),
              compoundTag.getString(DATA_DIALOG_YES_TAG),
              compoundTag.getString(DATA_DIALOG_NO_TAG));

    } else if (compoundTag.contains(DATA_DIALOG_DATA_TAG)
        && compoundTag.getCompound(DATA_DIALOG_DATA_TAG).contains(DATA_DIALOG_TYPE_TAG)) {
      log.info("Converting legacy dialog data to new multi-format for {}", this);
      CompoundTag dialogDataTag = compoundTag.getCompound(DATA_DIALOG_DATA_TAG);
      DialogType dialogType = DialogType.get(dialogDataTag.getString(DATA_DIALOG_TYPE_TAG));

      // Handle basic dialog
      if (dialogType == DialogType.BASIC) {
        dialogDataSet = DialogUtils.getBasicDialog(dialogDataTag.getString(DATA_DIALOG_SIMPLE_TAG));
        this.setDialogDataSet(dialogDataSet);
        return true;
      }

      // Handle only yes/no dialog
      if (dialogType != DialogType.YES_NO) {
        return false;
      }

      // Create yes/no dialog data set
      dialogDataSet =
          DialogUtils.getYesNoDialog(
              dialogDataTag.getString(DATA_DIALOG_SIMPLE_TAG),
              dialogDataTag.getString(DATA_DIALOG_YES_BUTTON_TAG),
              dialogDataTag.getString(DATA_DIALOG_NO_BUTTON_TAG),
              dialogDataTag.getString(DATA_DIALOG_YES_TAG),
              dialogDataTag.getString(DATA_DIALOG_NO_TAG));
    } else {
      return false;
    }

    // Handles possible legacy action data and covert them into button actions.
    if (compoundTag.contains(ActionEventData.DATA_ACTION_PERMISSION_LEVEL_TAG)
        && compoundTag.contains(ActionEventData.DATA_ACTION_DATA_TAG)) {
      log.info("Extracting legacy action data for button action for {}", this);
      DialogDataEntry questionDialogData = dialogDataSet.getDialog("question");
      ActionData legacyYesActionData =
          LegacyDialogSetData.readAdditionalLegacyActionData(
              compoundTag, dialogDataSet, ActionEventData.ON_YES_SELECTION);
      if (legacyYesActionData != null) {
        Set<ActionData> yesActionData = questionDialogData.getButton("yes_button").getActionData();
        if (legacyYesActionData.getType() == ActionType.OPEN_TRADING_SCREEN) {
          yesActionData.clear();
        }
        yesActionData.add(legacyYesActionData);
        questionDialogData.getButton("yes_button").setActionData(yesActionData);
      }
      ActionData legacyNoActionData =
          LegacyDialogSetData.readAdditionalLegacyActionData(
              compoundTag, dialogDataSet, ActionEventData.ON_NO_SELECTION);
      if (legacyNoActionData != null) {
        Set<ActionData> noActionData = questionDialogData.getButton("no_button").getActionData();
        if (legacyNoActionData.getType() == ActionType.OPEN_TRADING_SCREEN) {
          noActionData.clear();
        }
        noActionData.add(legacyNoActionData);
        questionDialogData.getButton("no_button").setActionData(noActionData);
      }
    }

    this.setDialogDataSet(dialogDataSet);
    return true;
  }
}
