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

package de.markusbordihn.easynpc.menu.configuration.dialog;

import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.menu.ModMenuTypes;
import java.util.UUID;
import javax.annotation.Nullable;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;

public class BasicDialogConfigurationMenu extends DialogConfigurationMenu {

  public BasicDialogConfigurationMenu(
      int windowId, Inventory playerInventory, UUID uuid, DialogDataSet dialogDataSet) {
    super(
        ModMenuTypes.BASIC_DIALOG_CONFIGURATION_MENU.get(),
        windowId,
        playerInventory,
        uuid,
        dialogDataSet);
  }

  public BasicDialogConfigurationMenu(
      int windowId, Inventory playerInventory, FriendlyByteBuf data) {
    this(windowId, playerInventory, data.readUUID(), new DialogDataSet(data.readNbt()));
  }

  public static MenuProvider getMenuProvider(
      UUID uuid, Entity entity, DialogDataSet dialogDataSet) {
    return new MenuProvider() {
      @Override
      public Component getDisplayName() {
        return Component.literal("Basic Dialog for " + entity.getName().getString());
      }

      @Nullable
      @Override
      public AbstractContainerMenu createMenu(
          int windowId, Inventory inventory, Player serverPlayer) {
        return new BasicDialogConfigurationMenu(windowId, inventory, uuid, dialogDataSet);
      }
    };
  }
}
