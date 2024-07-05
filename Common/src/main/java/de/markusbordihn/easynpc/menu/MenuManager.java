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
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import java.util.HashMap;
import java.util.Map;
import java.util.OptionalInt;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.MenuProvider;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class MenuManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final Map<UUID, MenuProvider> menuProviderMap = new HashMap<>();
  private static final Map<UUID, ServerPlayer> serverPlayerMap = new HashMap<>();
  private static final Map<UUID, CompoundTag> menuDataMap = new HashMap<>();
  private static final Map<Integer, UUID> dialogIdMap = new HashMap<>();

  private static MenuHandlerInterface menuHandlerInterface;

  private MenuManager() {}

  public static void registerMenuHandler(MenuHandlerInterface menuHandler) {
    menuHandlerInterface = menuHandler;
  }

  public static MenuHandlerInterface getMenuHandler() {
    return menuHandlerInterface;
  }

  public static void openMenu(
      UUID uuid, MenuProvider menuProvider, ServerPlayer serverPlayer, CompoundTag data) {
    UUID menuId = UUID.randomUUID();
    menuProviderMap.put(menuId, menuProvider);
    serverPlayerMap.put(menuId, serverPlayer);
    menuDataMap.put(menuId, data);
    NetworkMessageHandlerManager.getClientHandler().openMenu(uuid, menuId, serverPlayer, data);
  }

  public static void openMenu(UUID menuId) {
    MenuProvider menuProvider = menuProviderMap.get(menuId);
    ServerPlayer serverPlayer = serverPlayerMap.get(menuId);
    if (menuProvider != null && serverPlayer != null) {
      OptionalInt dialogId = serverPlayer.openMenu(menuProvider);
      if (dialogId.isPresent()) {
        dialogIdMap.put(dialogId.getAsInt(), menuId);
        log.debug(
            "Opened menu with ID {} ({}) and {} for {}",
            menuId,
            dialogId.getAsInt(),
            menuProvider,
            serverPlayer.getName().getString());
        menuProviderMap.remove(menuId);
        serverPlayerMap.remove(menuId);
        return;
      }
    }
    log.error("Unable to open menu with ID {}", menuId);
  }

  public static CompoundTag getMenuData(UUID menuId) {
    return menuDataMap.get(menuId);
  }
}
