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
import de.markusbordihn.easynpc.config.StorageConfig;
import de.markusbordihn.easynpc.data.status.StatusDataType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.function.ToLongFunction;
import java.util.stream.Stream;
import net.minecraft.server.MinecraftServer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class NPCSaveScheduler {

  static final float MAXIMUM_AVERAGE_TICK_MILLISECONDS = 40.0f;
  static final long MAXIMUM_RUN_NANOSECONDS = TimeUnit.MILLISECONDS.toNanos(2);
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[NPC Save Scheduler]";

  private static long nextRunTime;

  private NPCSaveScheduler() {}

  public static void handleServerTick(MinecraftServer minecraftServer) {
    long currentTime = System.currentTimeMillis();
    if (tryStartRun(currentTime, minecraftServer.getAverageTickTime())) {
      saveChangedNPCs(currentTime);
    }
  }

  static boolean tryStartRun(long currentTime, float averageTickMilliseconds) {
    if (StorageConfig.NPC_SAVE_INTERVAL_SECONDS <= 0) {
      nextRunTime = 0;
      return false;
    }

    if (nextRunTime == 0) {
      nextRunTime = currentTime + getIntervalMilliseconds();
      return false;
    }

    if (currentTime < nextRunTime
        || averageTickMilliseconds >= MAXIMUM_AVERAGE_TICK_MILLISECONDS) {
      return false;
    }

    nextRunTime = currentTime + getIntervalMilliseconds();
    return true;
  }

  public static int saveChangedNPCs(long currentTime) {
    List<EasyNPC<?>> selectedNPCs =
        selectOldestChanges(
            LivingEntityManager.getServerEasyNPCEntities().filter(NPCSaveScheduler::hasUnsavedData),
            NPCSaveScheduler::getLastUpdateTime,
            currentTime - TimeUnit.SECONDS.toMillis(StorageConfig.NPC_SAVE_MINIMUM_AGE_SECONDS),
            StorageConfig.NPC_SAVE_BATCH_SIZE);
    if (selectedNPCs.isEmpty()) {
      return 0;
    }

    long startTime = System.nanoTime();
    int savedNPCs = 0;
    for (EasyNPC<?> easyNPC : selectedNPCs) {
      if (NPCEntityManager.saveChangedNPCAndQueueFile(easyNPC)) {
        savedNPCs++;
      }
      if (System.nanoTime() - startTime > MAXIMUM_RUN_NANOSECONDS) {
        break;
      }
    }

    log.debug(
        "{} Saved {} of {} changed NPC(s) in {} ms.",
        LOG_PREFIX,
        savedNPCs,
        selectedNPCs.size(),
        TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startTime));
    return savedNPCs;
  }

  static <T> List<T> selectOldestChanges(
      Stream<T> changedCandidates,
      ToLongFunction<T> lastUpdateTimeOf,
      long latestAllowedUpdateTime,
      int batchSize) {
    return changedCandidates
        .filter(candidate -> lastUpdateTimeOf.applyAsLong(candidate) <= latestAllowedUpdateTime)
        .sorted(Comparator.comparingLong(lastUpdateTimeOf))
        .limit(batchSize)
        .toList();
  }

  public static void reset() {
    nextRunTime = 0;
  }

  private static long getIntervalMilliseconds() {
    return TimeUnit.SECONDS.toMillis(StorageConfig.NPC_SAVE_INTERVAL_SECONDS);
  }

  private static boolean hasUnsavedData(EasyNPC<?> easyNPC) {
    return easyNPC != null
        && easyNPC.isPersistentNPC()
        && easyNPC.getEasyNPCStatusData() != null
        && easyNPC.getEasyNPCStatusData().hasUnsavedNPCData()
        && !NPCEntityManager.isNPCDeleted(easyNPC.getEntityUUID());
  }

  private static long getLastUpdateTime(EasyNPC<?> easyNPC) {
    return easyNPC
        .getEasyNPCStatusData()
        .getStatusDataTimestamp(StatusDataType.NPC_DATA_LAST_UPDATE);
  }
}
