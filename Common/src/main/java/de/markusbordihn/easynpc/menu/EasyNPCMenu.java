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

package de.markusbordihn.easynpc.menu;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.screen.ScreenData;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCMenu extends AbstractContainerMenu {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected final Level level;
  protected final Player player;
  protected final ScreenData screenData;
  protected final EasyNPC<?> easyNPC;

  public EasyNPCMenu(
      final MenuType<?> menuType, final int containerId, final Inventory playerInventory) {
    this(menuType, containerId, playerInventory, new CompoundTag());
  }

  public EasyNPCMenu(
      final MenuType<?> menuType,
      final int containerId,
      final Inventory playerInventory,
      final CompoundTag data) {
    super(menuType, containerId);

    // Get player and level data.
    this.player = playerInventory.player;
    this.level = playerInventory.player.level();

    // Get additional menu and screen data, if available.
    CompoundTag menuData = this.level.isClientSide() ? ClientMenuManager.getMenuData() : data;
    this.screenData = ScreenData.decode(menuData);

    // Get easy NPC entity from screen data.
    this.easyNPC =
        this.level.isClientSide
            ? LivingEntityManager.getEasyNPCEntityByUUID(getNpcUUID())
            : LivingEntityManager.getEasyNPCEntityByUUID(getNpcUUID(), (ServerPlayer) player);
  }

  public ScreenData getScreenData() {
    return this.screenData;
  }

  public UUID getNpcUUID() {
    return this.screenData.uuid();
  }

  public int getPageIndex() {
    return this.screenData.pageIndex();
  }

  public EasyNPC<?> getEasyNPC() {
    return this.easyNPC;
  }

  @Override
  public boolean stillValid(Player player) {
    return player.isAlive();
  }

  @Override
  public ItemStack quickMoveStack(Player player, int slotIndex) {
    Slot slot = this.slots.get(slotIndex);
    if (!slot.hasItem()) {
      return ItemStack.EMPTY;
    }

    ItemStack itemStack = slot.getItem();

    // Store changes if itemStack is not empty.
    if (itemStack.isEmpty()) {
      slot.set(ItemStack.EMPTY);
    } else {
      slot.setChanged();
    }

    return ItemStack.EMPTY;
  }
}
