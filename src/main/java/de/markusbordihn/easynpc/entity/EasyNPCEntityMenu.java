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

package de.markusbordihn.easynpc.entity;

import java.util.UUID;
import javax.annotation.Nullable;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;

import net.minecraftforge.network.NetworkHooks;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.menu.DialogMenu;
import de.markusbordihn.easynpc.menu.configuration.MainConfigurationMenu;
import de.markusbordihn.easynpc.menu.ConfigurationMenu;

public class EasyNPCEntityMenu {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected EasyNPCEntityMenu() {}

  public static void openConfigurationMenu(ServerPlayer player, EasyNPCEntity entity) {
    log.debug("Open Easy NPC Configuration Menu for {} ...", entity);
    Component easyNPCName = entity.getCustomName();
    UUID uuid = entity.getUUID();
    MenuProvider provider = new MenuProvider() {
      @Override
      public Component getDisplayName() {
        return entity.getCustomName() != null ? easyNPCName : new TextComponent(uuid.toString());
      }

      @Nullable
      @Override
      public AbstractContainerMenu createMenu(int windowId, Inventory inventory, Player player) {
        return new ConfigurationMenu(windowId, inventory, uuid);
      }
    };

    NetworkHooks.openGui(player, provider, buffer -> buffer.writeUUID(uuid));
  }

  public static void openMainConfigurationMenu(ServerPlayer player, EasyNPCEntity entity) {
    log.debug("Open Easy NPC Main Configuration Menu for {} ...", entity);
    UUID uuid = entity.getUUID();
    MenuProvider provider = new MenuProvider() {
      @Override
      public Component getDisplayName() {
        return new TextComponent("Easy NPC (Non Player Character)");
      }

      @Nullable
      @Override
      public AbstractContainerMenu createMenu(int windowId, Inventory inventory, Player player) {
        return new MainConfigurationMenu(windowId, inventory, uuid);
      }
    };

    NetworkHooks.openGui(player, provider, buffer -> buffer.writeUUID(uuid));
  }

  public static void openDialogMenu(ServerPlayer player, EasyNPCEntity entity) {
    log.debug("Open Easy NPC Dialog Menu for {} ...", entity);
    Component easyNPCName = entity.getCustomName();
    UUID uuid = entity.getUUID();
    MenuProvider provider = new MenuProvider() {
      @Override
      public Component getDisplayName() {
        return entity.getCustomName() != null ? easyNPCName : new TextComponent(uuid.toString());
      }

      @Nullable
      @Override
      public AbstractContainerMenu createMenu(int windowId, Inventory inventory, Player player) {
        return new DialogMenu(windowId, inventory, uuid);
      }
    };

    NetworkHooks.openGui(player, provider, buffer -> buffer.writeUUID(uuid));
  }

}
