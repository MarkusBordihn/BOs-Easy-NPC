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
import java.util.Iterator;
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
  private static final long MENU_TIMEOUT_MS = 30000;

  private static final Map<UUID, MenuProvider> menuProviderMap = new ConcurrentHashMap<>();
  private static final Map<UUID, ServerPlayer> serverPlayerMap = new ConcurrentHashMap<>();
  private static final Map<UUID, UUID> menuNpcMap = new ConcurrentHashMap<>();
  private static final Map<UUID, Long> menuTimestampMap = new ConcurrentHashMap<>();

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
    UUID menuId = registerMenu(uuid, menuProvider, serverPlayer);
    NetworkMessageHandlerManager.getClientHandler().openMenu(uuid, menuId, serverPlayer, data);
  }

  public static UUID registerMenu(UUID uuid, MenuProvider menuProvider, ServerPlayer serverPlayer) {
    UUID menuId = UUID.randomUUID();
    menuProviderMap.put(menuId, menuProvider);
    serverPlayerMap.put(menuId, serverPlayer);
    menuNpcMap.put(menuId, uuid);
    menuTimestampMap.put(menuId, System.currentTimeMillis());
    cleanupExpiredMenus();
    return menuId;
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
    } else {
      log.error("Failed to open menu {} for {}", menuId, serverPlayer);
    }
    removeMenu(menuId);
  }

  private static void removeMenu(UUID menuId) {
    menuProviderMap.remove(menuId);
    serverPlayerMap.remove(menuId);
    menuNpcMap.remove(menuId);
    menuTimestampMap.remove(menuId);
  }

  public static void cleanupPlayerMenus(ServerPlayer serverPlayer) {
    if (serverPlayer == null) {
      return;
    }
    Iterator<Map.Entry<UUID, ServerPlayer>> iterator = serverPlayerMap.entrySet().iterator();
    while (iterator.hasNext()) {
      Map.Entry<UUID, ServerPlayer> entry = iterator.next();
      if (entry.getValue().equals(serverPlayer)) {
        UUID menuId = entry.getKey();
        log.debug("Cleaning up menu {} for disconnected player {}", menuId, serverPlayer);
        menuProviderMap.remove(menuId);
        menuNpcMap.remove(menuId);
        menuTimestampMap.remove(menuId);
        iterator.remove();
      }
    }
  }

  private static void cleanupExpiredMenus() {
    long currentTime = System.currentTimeMillis();
    Iterator<Map.Entry<UUID, Long>> iterator = menuTimestampMap.entrySet().iterator();
    while (iterator.hasNext()) {
      Map.Entry<UUID, Long> entry = iterator.next();
      if (currentTime - entry.getValue() > MENU_TIMEOUT_MS) {
        UUID menuId = entry.getKey();
        log.warn("Cleaning up expired menu {} after timeout", menuId);
        menuProviderMap.remove(menuId);
        serverPlayerMap.remove(menuId);
        menuNpcMap.remove(menuId);
        iterator.remove();
      }
    }
  }
}
