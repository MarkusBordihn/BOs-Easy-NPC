/*
 * Copyright 2026 Markus Bordihn
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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import de.markusbordihn.easynpc.data.npc.SavedNPCEntityEntry;
import de.markusbordihn.easynpc.data.saveddata.NPCEntityData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Collection;
import java.util.Collections;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.world.entity.Mob;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class NPCEntityManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final String LOG_PREFIX = "[NPC Entity Manager]";

  private static MinecraftServer currentServer;

  private NPCEntityManager() {}

  public static void initialize(MinecraftServer server) {
    if (server == null) {
      log.error("{} Cannot initialize with null server", LOG_PREFIX);
      return;
    }

    currentServer = server;
    NPCEntityData.init(server);
    log.info(
        "{} Initialized with {} tracked NPC entities", LOG_PREFIX, NPCEntityData.get().getCount());
  }

  public static <T extends Mob> void saveNPC(EasyNPC<T> easyNPC) {
    if (!validateServer() || easyNPC == null) {
      return;
    }

    easyNPC.getEasyNPCStatusData().markNPCDataUpdated();
    SavedNPCEntityEntry entry = SavedNPCEntityEntry.fromEasyNPC(easyNPC);
    if (entry != null) {
      getNPCEntityData().putEntry(entry.entityUUID(), entry);
      easyNPC.getEasyNPCStatusData().markNPCDataSaved();
      log.debug("{} Saved NPC entity: {}", LOG_PREFIX, entry.entityUUID());
    }
  }

  public static void removeNPC(UUID entityUUID) {
    if (!validateServer() || entityUUID == null) {
      return;
    }

    getNPCEntityData().removeEntry(entityUUID);
    log.debug("{} Removed NPC entity: {}", LOG_PREFIX, entityUUID);
  }

  public static void updateRemovalReason(UUID entityUUID, NPCRemovalReason reason) {
    if (!validateServer() || entityUUID == null) {
      return;
    }
    getNPCEntityData().updateRemovalReason(entityUUID, reason);
  }

  public static void saveAllDirtyNPCs() {
    if (!validateServer()) {
      return;
    }

    int savedCount = 0;
    for (EasyNPC<?> easyNPC : LivingEntityManager.getNpcEntityMap().values()) {
      if (easyNPC != null && easyNPC.getEasyNPCStatusData().hasUnsavedNPCData()) {
        saveNPC(easyNPC);
        savedCount++;
      }
    }

    if (savedCount > 0) {
      log.debug("{} Saved {} dirty NPC(s) to persistent storage", LOG_PREFIX, savedCount);
    }
  }

  public static Optional<SavedNPCEntityEntry> getNPC(UUID entityUUID) {
    if (!validateServer() || entityUUID == null) {
      return Optional.empty();
    }
    return getNPCEntityData().getEntry(entityUUID);
  }

  public static Collection<SavedNPCEntityEntry> getAllNPCs() {
    if (!validateServer()) {
      return Collections.emptyList();
    }
    return getNPCEntityData().getAllEntries();
  }

  public static int getCount() {
    if (!validateServer()) {
      return 0;
    }
    return getNPCEntityData().getCount();
  }

  public static Collection<SavedNPCEntityEntry> getNPCsByOwner(UUID ownerUUID) {
    if (!validateServer()) {
      return Collections.emptyList();
    }
    return getNPCEntityData().getEntriesByOwner(ownerUUID);
  }

  public static Collection<SavedNPCEntityEntry> getNPCsByType(String type) {
    if (!validateServer()) {
      return Collections.emptyList();
    }
    return getNPCEntityData().getEntriesByType(type);
  }

  public static Collection<SavedNPCEntityEntry> getNPCsByDimension(String dimension) {
    if (!validateServer()) {
      return Collections.emptyList();
    }
    return getNPCEntityData().getEntriesByDimension(dimension);
  }

  public static Collection<SavedNPCEntityEntry> getNPCsByPreset(UUID presetUUID) {
    if (!validateServer()) {
      return Collections.emptyList();
    }
    return getNPCEntityData().getEntriesByPreset(presetUUID);
  }

  public static Collection<SavedNPCEntityEntry> getNPCsByCustomIdentifier(
      ResourceLocation customIdentifier) {
    if (!validateServer()) {
      return Collections.emptyList();
    }
    return getNPCEntityData().getEntriesByCustomIdentifier(customIdentifier);
  }

  public static Collection<SavedNPCEntityEntry> getNPCsByCustomIdentifierNamespace(
      String namespace) {
    if (!validateServer()) {
      return Collections.emptyList();
    }
    return getNPCEntityData().getEntriesByCustomIdentifierNamespace(namespace);
  }

  private static boolean validateServer() {
    if (currentServer == null) {
      log.warn("{} Server not initialized", LOG_PREFIX);
      return false;
    }
    return true;
  }

  private static NPCEntityData getNPCEntityData() {
    return NPCEntityData.get();
  }
}
