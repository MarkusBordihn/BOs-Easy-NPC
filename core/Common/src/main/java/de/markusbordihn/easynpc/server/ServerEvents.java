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

package de.markusbordihn.easynpc.server;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.backup.BackupManager;
import de.markusbordihn.easynpc.condition.ConditionWarnings;
import de.markusbordihn.easynpc.data.preset.PresetReference;
import de.markusbordihn.easynpc.data.storage.NPCStorageStatistics;
import de.markusbordihn.easynpc.entity.EasyNPCEntityTypes;
import de.markusbordihn.easynpc.entity.NPCChurnTracker;
import de.markusbordihn.easynpc.entity.NPCEntityManager;
import de.markusbordihn.easynpc.entity.NPCSaveScheduler;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor.MessageActionExecutor;
import de.markusbordihn.easynpc.handler.EnvironmentChangeTracker;
import de.markusbordihn.easynpc.handler.OwnerLoginRestoreHandler;
import de.markusbordihn.easynpc.handler.PauseManager;
import de.markusbordihn.easynpc.handler.PlayerIdleTracker;
import de.markusbordihn.easynpc.handler.ShoulderNPCSynchronizer;
import de.markusbordihn.easynpc.io.BackgroundFileWriter;
import de.markusbordihn.easynpc.io.DataFileHandler;
import java.time.Duration;
import net.minecraft.server.MinecraftServer;
import net.minecraft.world.level.storage.LevelResource;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ServerEvents {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Duration PENDING_WRITES_SHUTDOWN_TIMEOUT = Duration.ofMinutes(2);

  private static NPCStorageStatistics storageStatisticsBeforeShutdown = NPCStorageStatistics.EMPTY;

  private ServerEvents() {}

  public static void handleServerStarting(MinecraftServer minecraftServer) {
    if (minecraftServer == null) {
      return;
    }

    log.info("{} Server is starting Events ...", Constants.LOG_REGISTER_PREFIX);

    Constants.WORLD_DIR = minecraftServer.getWorldPath(LevelResource.ROOT);

    BackupManager.reset();
    ConditionWarnings.reset();
    EnvironmentChangeTracker.reset();
    NPCChurnTracker.reset();
    NPCSaveScheduler.reset();
    PauseManager.reset();
    PresetReference.clearCache();
    MessageActionExecutor.clearKnownPlayerNames();
    DataFileHandler.registerServerDataFiles(minecraftServer);
  }

  public static void handleServerStarted(MinecraftServer minecraftServer) {
    if (minecraftServer == null) {
      return;
    }

    log.info("{} Server started Events ...", Constants.LOG_REGISTER_PREFIX);

    NPCEntityManager.ensureInitialized(minecraftServer);
  }

  public static void handleServerTick(MinecraftServer minecraftServer) {
    if (minecraftServer == null) {
      return;
    }

    BackupManager.performBackup();
    NPCSaveScheduler.handleServerTick(minecraftServer);

    OwnerLoginRestoreHandler.handleServerTick(minecraftServer);
    EnvironmentChangeTracker.handleServerTick(minecraftServer);
    PlayerIdleTracker.handleServerTick(minecraftServer);
    ShoulderNPCSynchronizer.handleServerTick(minecraftServer);
  }

  public static void handleServerStopping(MinecraftServer minecraftServer) {
    if (minecraftServer == null) {
      return;
    }

    log.info("{} Server is stopping, saving all dirty NPCs...", Constants.LOG_UNREGISTER_PREFIX);

    long startTime = System.currentTimeMillis();
    NPCEntityManager.saveAllDirtyNPCs();
    BackupManager.reset();
    EnvironmentChangeTracker.reset();
    NPCChurnTracker.reset();
    PauseManager.reset();
    PlayerIdleTracker.reset();
    ShoulderNPCSynchronizer.reset();
    PresetReference.clearCache();

    log.info(
        "{} Server stop handling finished in {} ms",
        Constants.LOG_UNREGISTER_PREFIX,
        System.currentTimeMillis() - startTime);
    storageStatisticsBeforeShutdown = NPCEntityManager.getStorageStatistics();
  }

  public static void handleServerStopped(MinecraftServer minecraftServer) {
    if (!BackgroundFileWriter.awaitPendingWrites(PENDING_WRITES_SHUTDOWN_TIMEOUT)) {
      log.warn(
          "{} {} NPC file write(s) were still pending after {} seconds.",
          Constants.LOG_UNREGISTER_PREFIX,
          BackgroundFileWriter.getPendingWriteCount(),
          PENDING_WRITES_SHUTDOWN_TIMEOUT.toSeconds());
    }
    logStorageStatistics(NPCEntityManager.getStorageStatistics());
    storageStatisticsBeforeShutdown = NPCStorageStatistics.EMPTY;
    NPCEntityManager.reset();
    BackupManager.reset();
    EasyNPCEntityTypes.reset();
  }

  private static void logStorageStatistics(NPCStorageStatistics statistics) {
    if (statistics.equals(NPCStorageStatistics.EMPTY)) {
      return;
    }

    log.info(
        "{} NPC files this session: {} written in {} ms, {} failed, {} read, {} unchanged unloads"
            + " skipped. Shutdown wrote {} file(s) in {} ms.",
        Constants.LOG_PREFIX,
        statistics.filesWritten(),
        statistics.fileWriteMilliseconds(),
        statistics.fileWriteFailures(),
        statistics.filesRead(),
        statistics.skippedUnchangedUnloads(),
        statistics.filesWritten() - storageStatisticsBeforeShutdown.filesWritten(),
        statistics.fileWriteMilliseconds()
            - storageStatisticsBeforeShutdown.fileWriteMilliseconds());
  }
}
