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

import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.server.ServerDataAccessor;
import de.markusbordihn.easynpc.data.server.ServerDataIndex;
import de.markusbordihn.easynpc.data.server.ServerEntityData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.menu.MenuManager;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.PathfinderMob;

public interface DialogData<T extends PathfinderMob> extends EasyNPC<T> {

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

  default void openDialogMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC, UUID dialogId, int pageIndex) {
    MenuManager.getMenuHandler().openDialogMenu(serverPlayer, easyNPC, dialogId, pageIndex);
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

  default void openDialog(ServerPlayer serverPlayer) {
    openDialog(serverPlayer, getDialogDataSet().getDefaultDialogId());
  }

  default void openDialog(ServerPlayer serverPlayer, UUID dialogId) {
    MenuManager.getMenuHandler().openDialogMenu(serverPlayer, this, dialogId, 0);
  }

  default DialogButtonEntry getDialogButton(UUID dialogId, UUID dialogButtonId) {
    return getDialogDataSet().getDialogButton(dialogId, dialogButtonId);
  }

  default void defineSynchedDialogData(SynchedEntityData.Builder builder) {}

  default void defineCustomDialogData() {
    getEasyNPCServerData().defineServerEntityData(CUSTOM_DATA_DIALOG_DATA_SET, new DialogDataSet());
  }

  default void addAdditionalDialogData(CompoundTag compoundTag) {
    CompoundTag dialogDataTag = new CompoundTag();

    if (this.isServerSide()) {
      DialogDataSet dialogDataSet = this.getDialogDataSet();
      if (dialogDataSet != null) {
        dialogDataSet.save(dialogDataTag);
      }
    }

    compoundTag.put(DATA_DIALOG_DATA_TAG, dialogDataTag);
  }

  default void readAdditionalDialogData(CompoundTag compoundTag) {

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
}
