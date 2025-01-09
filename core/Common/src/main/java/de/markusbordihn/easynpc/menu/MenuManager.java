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
import java.util.Map;
import java.util.OptionalInt;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.MenuProvider;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class MenuManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final Map<UUID, MenuProvider> menuProviderMap = new ConcurrentHashMap<>();
  private static final Map<UUID, ServerPlayer> serverPlayerMap = new ConcurrentHashMap<>();
  private static final Map<UUID, UUID> menuNpcMap = new ConcurrentHashMap<>();

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
    menuNpcMap.put(menuId, uuid);
    NetworkMessageHandlerManager.getClientHandler().openMenu(uuid, menuId, serverPlayer, data);
  }

  public static void openMenu(final UUID menuId, final ServerPlayer serverPlayer) {
    // Verify if the menu is still available for the player.
    ServerPlayer menuServerPlayer = serverPlayerMap.get(menuId);
    if (menuServerPlayer == null || !menuServerPlayer.equals(serverPlayer)) {
      log.error(
          "Invalid server player ({} != {}) for menu {}", serverPlayer, menuServerPlayer, menuId);
      return;
    }

    // Validate the menu provider
    MenuProvider menuProvider = menuProviderMap.get(menuId);
    if (menuProvider == null) {
      log.error("Invalid menu provider for menu {}", menuId);
      return;
    }

    // Validate NPC UUID
    UUID npcUUID = menuNpcMap.get(menuId);
    if (npcUUID == null) {
      log.error("Invalid NPC UUID for menu {}", menuId);
      return;
    }

    // Open the menu for the player
    log.info(
        "Opening menu {} for npc {} and player {} with {}",
        menuId,
        npcUUID,
        serverPlayer,
        menuProvider);
    OptionalInt dialogId = serverPlayer.openMenu(menuProvider);
    if (dialogId.isPresent()) {
      log.debug(
          "Opened menu {} ({}) and {} for {}",
          menuId,
          dialogId.getAsInt(),
          menuProvider,
          serverPlayer);
      menuProviderMap.remove(menuId);
      serverPlayerMap.remove(menuId);
    } else {
      log.error("Got invalid dialog ID for menu {}", menuId);
    }
  }
}
