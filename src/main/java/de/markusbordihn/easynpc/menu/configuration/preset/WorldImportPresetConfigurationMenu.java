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

package de.markusbordihn.easynpc.menu.configuration.preset;

import java.util.List;
import java.util.UUID;
import javax.annotation.Nullable;

import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;

import de.markusbordihn.easynpc.menu.ModMenuTypes;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;

public class WorldImportPresetConfigurationMenu extends ConfigurationMenu {

  // Cache
  protected final List<ResourceLocation> worldPresets;

  public WorldImportPresetConfigurationMenu(int windowId, Inventory playerInventory, UUID uuid,
      List<ResourceLocation> worldPresets) {
    super(ModMenuTypes.WORLD_IMPORT_PRESET_CONFIGURATION_MENU.get(), windowId, playerInventory,
        uuid);
    this.worldPresets = worldPresets;
  }

  public WorldImportPresetConfigurationMenu(int windowId, Inventory playerInventory,
      FriendlyByteBuf data) {
    this(windowId, playerInventory, data.readUUID(),
        data.readList(FriendlyByteBuf::readResourceLocation));
  }

  public List<ResourceLocation> getWorldPresets() {
    return this.worldPresets;
  }

  public static MenuProvider getMenuProvider(UUID uuid, Entity entity,
      List<ResourceLocation> worldPresets) {
    return new MenuProvider() {
      @Override
      public Component getDisplayName() {
        return Component.literal("World Import preset for " + entity.getName().getString());
      }

      @Nullable
      @Override
      public AbstractContainerMenu createMenu(int windowId, Inventory inventory,
          Player serverPlayer) {
        return new WorldImportPresetConfigurationMenu(windowId, inventory, uuid, worldPresets);
      }
    };
  }

}
