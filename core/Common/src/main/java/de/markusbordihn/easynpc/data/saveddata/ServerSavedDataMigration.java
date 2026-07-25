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

package de.markusbordihn.easynpc.data.saveddata;

import de.markusbordihn.easynpc.Constants;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.saveddata.SavedData;
import net.minecraft.world.level.saveddata.SavedDataType;
import net.minecraft.world.level.storage.SavedDataStorage;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class ServerSavedDataMigration {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ServerSavedDataMigration() {}

  public static <T extends SavedData> T getOrMigrateFromOverworld(
      MinecraftServer server, SavedDataType<T> savedDataType) {
    SavedDataStorage serverStorage = server.getDataStorage();
    T serverData = serverStorage.get(savedDataType);
    if (serverData != null) {
      return serverData;
    }

    T legacyData = readLegacyOverworldData(server, savedDataType);
    if (legacyData == null) {
      return serverStorage.computeIfAbsent(savedDataType);
    }

    serverStorage.set(savedDataType, legacyData);
    log.info(
        "Migrated {} from the overworld to the server data storage, legacy file kept as backup",
        savedDataType.id());
    return legacyData;
  }

  private static <T extends SavedData> T readLegacyOverworldData(
      MinecraftServer server, SavedDataType<T> savedDataType) {
    ServerLevel overworld = server.overworld();
    if (overworld == null) {
      return null;
    }

    return overworld.getDataStorage().get(savedDataType);
  }
}
