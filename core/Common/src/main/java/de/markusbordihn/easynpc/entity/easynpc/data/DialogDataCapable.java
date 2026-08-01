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

import de.markusbordihn.easynpc.api.event.EasyNPCEventRegistry;
import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.dialog.DialogType;
import de.markusbordihn.easynpc.data.server.ServerDataAccessor;
import de.markusbordihn.easynpc.data.server.ServerDataIndex;
import de.markusbordihn.easynpc.data.server.ServerEntityData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.menu.MenuManager;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.level.storage.ValueInput;
import net.minecraft.world.level.storage.ValueOutput;

public interface DialogDataCapable<T extends Mob> extends EasyNPC<T> {

  ServerDataAccessor<DialogDataSet> CUSTOM_DATA_DIALOG_DATA_SET =
      ServerEntityData.defineId(
          ServerDataIndex.DIALOG_DATA_SET, EntityDataSerializersManager.DIALOG_DATA_SET);
  String DATA_DIALOG_DATA_TAG = "DialogData";

  default int getEntityDialogTop() {
    return 0;
  }

  default int getEntityDialogLeft() {
    return 0;
  }

  default int getEntityDialogScaling() {
    return 50;
  }

  default DialogDataSet getDialogDataSet() {
    return getEasyNPCServerData().getServerEntityData(CUSTOM_DATA_DIALOG_DATA_SET);
  }

  default void setDialogDataSet(DialogDataSet dialogDataSet) {
    getEasyNPCServerData().setServerEntityData(CUSTOM_DATA_DIALOG_DATA_SET, dialogDataSet);
  }

  default void clearDialogDataSet() {
    setDialogDataSet(new DialogDataSet());
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

  default void openDialog(ServerPlayer serverPlayer, UUID dialogId) {
    DialogDataEntry dialog = getDialogDataSet().getDialog(dialogId);
    if (dialog != null) {
      getDialogDataSet().recordDialogExecution(dialog, serverPlayer, this.getLivingEntity());
    }
    MenuManager.getMenuHandler().openDialogMenu(serverPlayer, this, dialogId, 0);
    EasyNPCEventRegistry.fireDialogOpened(this, serverPlayer, dialog);
  }

  default boolean openDialogIfConditionsMet(ServerPlayer serverPlayer, UUID dialogId) {
    if (!getDialogDataSet().canOpenDialog(dialogId, serverPlayer, this.getLivingEntity())) {
      return false;
    }
    openDialog(serverPlayer, dialogId);
    return true;
  }

  default void openDefaultDialog(ServerPlayer serverPlayer) {
    DialogDataEntry dialog =
        getDialogDataSet().getNextAvailableDialog(serverPlayer, this.getLivingEntity());
    if (dialog != null) {
      this.openDialog(serverPlayer, dialog.getId());
    }
  }

  default DialogButtonEntry getDialogButton(UUID dialogId, UUID dialogButtonId) {
    return getDialogDataSet().getDialogButton(dialogId, dialogButtonId);
  }

  default void defineSynchedDialogData(SynchedEntityData.Builder builder) {}

  default void defineCustomDialogData() {
    getEasyNPCServerData().defineServerEntityData(CUSTOM_DATA_DIALOG_DATA_SET, new DialogDataSet());
  }

  default void addAdditionalDialogData(ValueOutput valueOutput) {
    if (this.isServerSideInstance()) {
      DialogDataSet dialogDataSet = this.getDialogDataSet();
      if (dialogDataSet != null && dialogDataSet.hasDialog()) {
        CompoundTag dialogDataTag = new CompoundTag();
        dialogDataSet.save(dialogDataTag);
        valueOutput.store(DATA_DIALOG_DATA_TAG, CompoundTag.CODEC, dialogDataTag);
      }
    }
  }

  default void readAdditionalDialogData(ValueInput valueInput) {
    // Early exit if no dialog data is available.
    Optional<CompoundTag> compoundTagData =
        valueInput.read(DATA_DIALOG_DATA_TAG, CompoundTag.CODEC);
    if (compoundTagData.isEmpty()) {
      DialogDataSet dialogDataSet = this.getDialogDataSet();
      if (dialogDataSet != null && !dialogDataSet.hasDialog()) {
        this.setDialogDataSet(new DialogDataSet(DialogType.NONE));
      }
      return;
    }

    CompoundTag dialogDataTag = compoundTagData.get();

    if (dialogDataTag.contains(DialogDataSet.DATA_DIALOG_DATA_SET_TAG)) {
      DialogDataSet dialogDataSet = new DialogDataSet(dialogDataTag);
      this.setDialogDataSet(dialogDataSet);
    }
  }
}
