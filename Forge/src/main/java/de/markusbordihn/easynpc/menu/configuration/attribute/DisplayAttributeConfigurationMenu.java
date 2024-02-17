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

package de.markusbordihn.easynpc.menu.configuration.attribute;

import de.markusbordihn.easynpc.menu.ModMenuTypes;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;

public class DisplayAttributeConfigurationMenu extends AttributeConfigurationMenu {

  public DisplayAttributeConfigurationMenu(int windowId, Inventory playerInventory, UUID uuid) {
    super(ModMenuTypes.DISPLAY_ATTRIBUTE_CONFIGURATION_MENU.get(), windowId, playerInventory, uuid);
  }

  public DisplayAttributeConfigurationMenu(
      int windowId, Inventory playerInventory, FriendlyByteBuf data) {
    this(windowId, playerInventory, data.readUUID());
  }

  public static MenuProvider getMenuProvider(UUID uuid, Entity entity) {
    return new MenuProvider() {
      @Override
      public Component getDisplayName() {
        return Component.literal("Display Attribute for " + entity.getName().getString());
      }

      @Override
      public AbstractContainerMenu createMenu(
          int windowId, Inventory inventory, Player serverPlayer) {
        return new DisplayAttributeConfigurationMenu(windowId, inventory, uuid);
      }
    };
  }
}
