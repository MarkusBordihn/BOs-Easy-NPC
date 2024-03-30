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

package de.markusbordihn.easynpc.menu.dialog;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.menu.ModMenuTypes;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.MenuType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DialogMenu extends AbstractContainerMenu {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  // Cache
  protected final ActionEventSet actionDataSet;
  protected final DialogDataSet dialogDataSet;
  protected final Inventory playerInventory;
  protected final EasyNPC<?> easyNPC;
  protected final UUID dialogId;
  protected final UUID uuid;
  protected final int pageIndex;

  public DialogMenu(
      int windowId,
      Inventory playerInventory,
      UUID uuid,
      ActionEventSet actionsDataSet,
      DialogDataSet dialogDataSet,
      UUID dialogId,
      int pageIndex) {
    this(
        ModMenuTypes.DIALOG_MENU.get(),
        windowId,
        playerInventory,
        uuid,
        actionsDataSet,
        dialogDataSet,
        dialogId,
        pageIndex);
  }

  public DialogMenu(int windowId, Inventory playerInventory, FriendlyByteBuf data) {
    this(
        windowId,
        playerInventory,
        data.readUUID(),
        new ActionEventSet(data.readNbt()),
        new DialogDataSet(data.readNbt()),
        data.readUUID(),
        data.readInt());
  }

  public DialogMenu(
      final MenuType<?> menuType,
      final int windowId,
      final Inventory playerInventory,
      UUID uuid,
      ActionEventSet actionDataSet,
      DialogDataSet dialogDataSet,
      UUID dialogId,
      int pageIndex) {
    super(menuType, windowId);
    this.playerInventory = playerInventory;
    this.actionDataSet = actionDataSet;
    this.dialogDataSet = dialogDataSet;
    this.uuid = uuid;
    this.dialogId = dialogId;
    this.pageIndex = pageIndex;

    // Get entity from cache
    this.easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid);

    log.debug(
        "Open Dialog menu for {}: {} with player inventory {} and actions {} and dialogs {}",
        this.uuid,
        this.easyNPC,
        playerInventory,
        this.actionDataSet,
        this.dialogDataSet);
  }

  public static MenuProvider getMenuProvider(
      UUID uuid,
      Entity entity,
      ActionEventSet actionDataSet,
      DialogDataSet dialogDataSet,
      UUID dialogId,
      int pageIndex) {
    return new MenuProvider() {
      @Override
      public Component getDisplayName() {
        return entity.getName();
      }

      @Override
      public AbstractContainerMenu createMenu(
          int windowId, Inventory inventory, Player serverPlayer) {
        return new DialogMenu(
            windowId, inventory, uuid, actionDataSet, dialogDataSet, dialogId, pageIndex);
      }
    };
  }

  public ActionEventSet getActionEventSet() {
    return this.actionDataSet;
  }

  public DialogDataSet getDialogDataSet() {
    return this.dialogDataSet;
  }

  public EasyNPC<?> getEasyNPC() {
    return this.easyNPC;
  }

  public UUID getUUID() {
    return this.uuid;
  }

  public UUID getDialogId() {
    return this.dialogId;
  }

  public int getPageIndex() {
    return this.pageIndex;
  }

  public Inventory getPlayerInventory() {
    return this.playerInventory;
  }

  @Override
  public boolean stillValid(Player player) {
    return player.isAlive() && this.easyNPC != null && this.easyNPC.getEntity().isAlive();
  }
}
