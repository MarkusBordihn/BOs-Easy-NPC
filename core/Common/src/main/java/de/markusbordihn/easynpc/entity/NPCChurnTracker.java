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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.Mob;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class NPCChurnTracker {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final String LOG_PREFIX = "[NPC Churn Tracker]";

  private static final long MEASURE_WINDOW_MILLIS = 60_000L;
  private static final long WARNING_COOLDOWN_MILLIS = 600_000L;
  private static final int UNLOAD_WARNING_THRESHOLD = 20;
  private static final int MAX_TRACKED_ENTITIES = 1024;

  private static final Map<UUID, ChurnRecord> unloadRecords = new ConcurrentHashMap<>();

  private NPCChurnTracker() {}

  public static void reset() {
    unloadRecords.clear();
  }

  public static <T extends Mob> void trackChunkUnload(EasyNPC<T> easyNPC) {
    if (easyNPC == null) {
      return;
    }

    UUID entityUUID = easyNPC.getEntityUUID();
    if (entityUUID == null) {
      return;
    }

    long currentTimeMillis = System.currentTimeMillis();
    if (unloadRecords.size() >= MAX_TRACKED_ENTITIES) {
      unloadRecords.values().removeIf(record -> record.isExpired(currentTimeMillis));
      if (unloadRecords.size() >= MAX_TRACKED_ENTITIES && !unloadRecords.containsKey(entityUUID)) {
        return;
      }
    }

    ChurnRecord updatedRecord =
        unloadRecords.compute(
            entityUUID,
            (uuid, record) -> {
              if (record == null) {
                return new ChurnRecord(currentTimeMillis, 1, 0L);
              }
              if (record.isExpired(currentTimeMillis)) {
                return new ChurnRecord(currentTimeMillis, 1, record.lastWarningTimeMillis());
              }

              return record.withAdditionalUnload();
            });

    if (updatedRecord.unloadCount() < UNLOAD_WARNING_THRESHOLD
        || currentTimeMillis - updatedRecord.lastWarningTimeMillis() < WARNING_COOLDOWN_MILLIS) {
      return;
    }

    unloadRecords.put(entityUUID, new ChurnRecord(currentTimeMillis, 0, currentTimeMillis));
    Entity entity = easyNPC.getEntity();
    log.warn(
        "{} {} ({}) was unloaded {} times within {} seconds at {} in chunk {} of {}! "
            + "Check your chunk loaders and other mods, a chunk which is constantly loaded and "
            + "unloaded costs performance and prevents the NPC from acting.",
        LOG_PREFIX,
        easyNPC,
        entityUUID,
        updatedRecord.unloadCount(),
        MEASURE_WINDOW_MILLIS / 1000L,
        entity.blockPosition(),
        entity.chunkPosition(),
        entity.level().dimension().identifier());
  }

  private record ChurnRecord(
      long windowStartTimeMillis, int unloadCount, long lastWarningTimeMillis) {

    private boolean isExpired(long currentTimeMillis) {
      return currentTimeMillis - this.windowStartTimeMillis > MEASURE_WINDOW_MILLIS;
    }

    private ChurnRecord withAdditionalUnload() {
      return new ChurnRecord(
          this.windowStartTimeMillis, this.unloadCount + 1, this.lastWarningTimeMillis);
    }
  }
}
