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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.config.StorageConfig;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.npc.NPCEntityMetadata;
import de.markusbordihn.easynpc.data.saveddata.NPCEntityData;
import de.markusbordihn.easynpc.data.status.StatusDataType;
import de.markusbordihn.easynpc.entity.NPCEntityManager;
import de.markusbordihn.easynpc.entity.NPCSaveScheduler;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.io.BackgroundFileWriter;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class NPCSaveSchedulerTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 PLAYER_POSITION = new Vec3(2, 2, 1);
  private static final Duration WRITE_TIMEOUT = Duration.ofSeconds(30);
  private static final int MINIMUM_AGE_SECONDS = 30;
  private static final long AFTER_MINIMUM_AGE_MILLISECONDS =
      TimeUnit.SECONDS.toMillis(MINIMUM_AGE_SECONDS + 1L);
  private static final int MAXIMUM_SCHEDULER_RUNS = 100;
  private static final int LIMITED_BATCH_SIZE = 2;
  private static final int BATCH_TEST_NPC_COUNT = 6;
  private static final String CUSTOM_NAME_TAG = "CustomName";
  private static final String FIRST_NAME = "Scheduled Storage NPC";
  private static final String SECOND_NAME = "Unloaded Storage NPC";

  private NPCSaveSchedulerTestHelper() {}

  public static void assertChangeIsSavedAfterMinimumAge(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    UUID entityUUID = easyNPC.getEntityUUID();
    long filesWrittenBefore = NPCEntityManager.getStorageStatistics().filesWritten();
    easyNPC.getEntity().setCustomName(Component.literal(FIRST_NAME));
    long changeTime = System.currentTimeMillis();

    runWithSchedulerConfig(
        StorageConfig.NPC_SAVE_INTERVAL_SECONDS,
        StorageConfig.NPC_SAVE_BATCH_SIZE,
        MINIMUM_AGE_SECONDS,
        () -> {
          NPCSaveScheduler.saveChangedNPCs(changeTime);
          GameTestHelpers.assertTrue(
              helper,
              "A change younger than the minimum age must not be saved",
              easyNPC.getEasyNPCStatusData().hasUnsavedNPCData());
          runSchedulerUntilSaved(helper, easyNPC, changeTime + AFTER_MINIMUM_AGE_MILLISECONDS);
        });

    awaitWrites(helper);
    GameTestHelpers.assertTrue(
        helper,
        "The scheduled save must write the NPC file",
        !NPCEntityManager.isNPCFileDirty(entityUUID));
    GameTestHelpers.assertTrue(
        helper,
        "The written NPC file must contain the change",
        getStoredCustomName(entityUUID).contains(FIRST_NAME));
    GameTestHelpers.assertTrue(
        helper,
        "The scheduled save must count the written NPC file",
        NPCEntityManager.getStorageStatistics().filesWritten() > filesWrittenBefore);
  }

  public static void assertUnchangedNPCIsNotSaved(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    long savedLastUpdate = getLastUpdate(easyNPC);

    NPCSaveScheduler.saveChangedNPCs(System.currentTimeMillis() + AFTER_MINIMUM_AGE_MILLISECONDS);

    GameTestHelpers.assertEquals(
        helper,
        "The scheduler must not change the last update of an unchanged NPC",
        savedLastUpdate,
        getLastUpdate(easyNPC));
    GameTestHelpers.assertTrue(
        helper,
        "The scheduler must not queue a write for an unchanged NPC",
        !NPCEntityManager.isNPCFileDirty(easyNPC.getEntityUUID()));
  }

  public static void assertBatchSizeLimitsSavesPerRun(
      GameTestHelper helper, EntityType<?> entityType) {
    List<EasyNPC<?>> changedNPCs = new ArrayList<>(BATCH_TEST_NPC_COUNT);
    for (int i = 0; i < BATCH_TEST_NPC_COUNT; i++) {
      EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
      easyNPC.getEntity().setCustomName(Component.literal(FIRST_NAME + " " + i));
      changedNPCs.add(easyNPC);
    }
    long futureTime = System.currentTimeMillis() + AFTER_MINIMUM_AGE_MILLISECONDS;

    runWithSchedulerConfig(
        StorageConfig.NPC_SAVE_INTERVAL_SECONDS,
        LIMITED_BATCH_SIZE,
        MINIMUM_AGE_SECONDS,
        () -> {
          int savedNPCs = NPCSaveScheduler.saveChangedNPCs(futureTime);
          GameTestHelpers.assertTrue(
              helper,
              "One run must save at most " + LIMITED_BATCH_SIZE + " NPCs, but saved " + savedNPCs,
              savedNPCs <= LIMITED_BATCH_SIZE);
          long unsavedNPCs =
              changedNPCs.stream()
                  .filter(easyNPC -> easyNPC.getEasyNPCStatusData().hasUnsavedNPCData())
                  .count();
          GameTestHelpers.assertTrue(
              helper,
              "One run must leave the remaining changed NPCs for later runs",
              unsavedNPCs >= BATCH_TEST_NPC_COUNT - LIMITED_BATCH_SIZE);

          for (EasyNPC<?> easyNPC : changedNPCs) {
            runSchedulerUntilSaved(helper, easyNPC, futureTime);
          }
        });
    awaitWrites(helper);
  }

  public static void assertUnloadDuringPendingWriteKeepsNewestData(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    UUID entityUUID = easyNPC.getEntityUUID();
    easyNPC.getEntity().setCustomName(Component.literal(FIRST_NAME));
    runSchedulerUntilSaved(
        helper, easyNPC, System.currentTimeMillis() + AFTER_MINIMUM_AGE_MILLISECONDS);

    easyNPC.getEntity().setCustomName(Component.literal(SECOND_NAME));
    EasyNPC<?> reloadedNPC = reloadFromChunk(helper, easyNPC);
    if (reloadedNPC == null) {
      return;
    }

    NPCEntityData.get().saveAllDirtyNPCs();
    awaitWrites(helper);
    GameTestHelpers.assertTrue(
        helper,
        "The NPC file must contain the change made after the scheduled save",
        getStoredCustomName(entityUUID).contains(SECOND_NAME));
    GameTestHelpers.assertTrue(
        helper,
        "The NPC loaded from its chunk must contain the newest change",
        reloadedNPC.getEntity().getCustomName() != null
            && SECOND_NAME.equals(reloadedNPC.getEntity().getCustomName().getString()));
  }

  public static void assertAutosaveWritesChangedNPCFile(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    UUID entityUUID = easyNPC.getEntityUUID();
    easyNPC.getEntity().setCustomName(Component.literal(FIRST_NAME));
    easyNPC.getEntity().setRemoved(Entity.RemovalReason.UNLOADED_TO_CHUNK);
    GameTestHelpers.assertTrue(
        helper,
        "Unloading a changed NPC must mark its NPC file as dirty",
        NPCEntityManager.isNPCFileDirty(entityUUID));

    NPCEntityData.get().save(new CompoundTag());
    awaitWrites(helper);
    GameTestHelpers.assertTrue(
        helper,
        "The autosave must write the dirty NPC file",
        !NPCEntityManager.isNPCFileDirty(entityUUID));
    GameTestHelpers.assertTrue(
        helper,
        "The NPC file written by the autosave must contain the change",
        getStoredCustomName(entityUUID).contains(FIRST_NAME));
  }

  public static void assertDisabledIntervalKeepsChangeUntilUnload(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    UUID entityUUID = easyNPC.getEntityUUID();
    easyNPC.getEntity().setCustomName(Component.literal(FIRST_NAME));

    runWithSchedulerConfig(
        0,
        StorageConfig.NPC_SAVE_BATCH_SIZE,
        0,
        () -> {
          NPCSaveScheduler.handleServerTick(helper.getLevel().getServer());
          NPCSaveScheduler.handleServerTick(helper.getLevel().getServer());
        });

    GameTestHelpers.assertTrue(
        helper,
        "A disabled save interval must keep the change unsaved",
        easyNPC.getEasyNPCStatusData().hasUnsavedNPCData());
    easyNPC.getEntity().setRemoved(Entity.RemovalReason.UNLOADED_TO_CHUNK);
    GameTestHelpers.assertTrue(
        helper,
        "Unloading must still save the change with a disabled save interval",
        NPCEntityManager.isNPCFileDirty(entityUUID));
  }

  public static void assertMissingIndexHashIsAdoptedWithoutWrite(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    UUID entityUUID = easyNPC.getEntityUUID();
    long savedLastUpdate = getLastUpdate(easyNPC);
    long storedHash = getStoredHash(entityUUID);
    GameTestHelpers.assertTrue(helper, "A saved NPC must have an index hash", storedHash != 0L);

    NPCEntityData.get().updateNPCDataHash(entityUUID, 0L);

    GameTestHelpers.assertTrue(
        helper,
        "A missing index hash alone must not save the NPC",
        !NPCEntityManager.saveChangedNPCAndQueueFile(easyNPC));
    GameTestHelpers.assertEquals(
        helper, "The missing index hash must be adopted", storedHash, getStoredHash(entityUUID));
    GameTestHelpers.assertEquals(
        helper,
        "Adopting the index hash must not change the last update",
        savedLastUpdate,
        getLastUpdate(easyNPC));
    GameTestHelpers.assertTrue(
        helper,
        "Adopting the index hash must not queue a write of the NPC file",
        !NPCEntityManager.isNPCFileDirty(entityUUID));
  }

  public static void assertFiredSpawnActionIsSaved(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    UUID entityUUID = easyNPC.getEntityUUID();
    ActionDataSet actionDataSet = new ActionDataSet();
    actionDataSet.add(
        new ActionDataEntry(ActionDataType.NPC_STATE, "increase gametest_spawn_action 1"));
    ActionEventSet actionEventSet = new ActionEventSet();
    actionEventSet.setActionEvent(ActionEventType.ON_SPAWN, actionDataSet);
    easyNPC.getEasyNPCActionEventData().setActionEventSet(actionEventSet);
    saveAndFlush(easyNPC);
    long hashBeforeSpawnAction = getStoredHash(entityUUID);

    GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION);
    easyNPC.getEasyNPCActionHandler().checkSpawnAction();
    GameTestHelpers.assertTrue(
        helper,
        "A player near the NPC must fire the spawn action",
        easyNPC.getEasyNPCStatusData().getStatusDataFlag(StatusDataType.SPAWN_ACTION_FIRED));
    GameTestHelpers.assertTrue(
        helper,
        "A fired spawn action must mark the NPC as changed",
        easyNPC.getEasyNPCStatusData().hasUnsavedNPCData());

    runSchedulerUntilSaved(
        helper, easyNPC, System.currentTimeMillis() + AFTER_MINIMUM_AGE_MILLISECONDS);
    awaitWrites(helper);
    GameTestHelpers.assertTrue(
        helper,
        "The fired spawn action must be written to the NPC file",
        getStoredHash(entityUUID) != hashBeforeSpawnAction
            && !NPCEntityManager.isNPCFileDirty(entityUUID));
  }

  private static void runWithSchedulerConfig(
      int intervalSeconds, int batchSize, int minimumAgeSeconds, Runnable schedulerTest) {
    int previousIntervalSeconds = StorageConfig.NPC_SAVE_INTERVAL_SECONDS;
    int previousBatchSize = StorageConfig.NPC_SAVE_BATCH_SIZE;
    int previousMinimumAgeSeconds = StorageConfig.NPC_SAVE_MINIMUM_AGE_SECONDS;
    StorageConfig.NPC_SAVE_INTERVAL_SECONDS = intervalSeconds;
    StorageConfig.NPC_SAVE_BATCH_SIZE = batchSize;
    StorageConfig.NPC_SAVE_MINIMUM_AGE_SECONDS = minimumAgeSeconds;
    try {
      schedulerTest.run();
    } finally {
      StorageConfig.NPC_SAVE_INTERVAL_SECONDS = previousIntervalSeconds;
      StorageConfig.NPC_SAVE_BATCH_SIZE = previousBatchSize;
      StorageConfig.NPC_SAVE_MINIMUM_AGE_SECONDS = previousMinimumAgeSeconds;
    }
  }

  private static void runSchedulerUntilSaved(
      GameTestHelper helper, EasyNPC<?> easyNPC, long currentTime) {
    for (int run = 0;
        run < MAXIMUM_SCHEDULER_RUNS && easyNPC.getEasyNPCStatusData().hasUnsavedNPCData();
        run++) {
      NPCSaveScheduler.saveChangedNPCs(currentTime);
    }
    GameTestHelpers.assertTrue(
        helper,
        "The scheduler must save the changed NPC " + easyNPC.getEntityUUID(),
        !easyNPC.getEasyNPCStatusData().hasUnsavedNPCData());
  }

  private static void awaitWrites(GameTestHelper helper) {
    GameTestHelpers.assertTrue(
        helper,
        "The background file writer must finish within " + WRITE_TIMEOUT,
        BackgroundFileWriter.awaitPendingWrites(WRITE_TIMEOUT));
  }

  private static EasyNPC<?> spawnSavedNPC(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    saveAndFlush(easyNPC);
    GameTestHelpers.assertTrue(
        helper,
        "A freshly saved NPC must not be marked as changed",
        !easyNPC.getEasyNPCStatusData().hasUnsavedNPCData());
    return easyNPC;
  }

  private static void saveAndFlush(EasyNPC<?> easyNPC) {
    NPCEntityManager.saveNPC(easyNPC);
    NPCEntityData.get().saveAllDirtyNPCs();
  }

  private static EasyNPC<?> reloadFromChunk(GameTestHelper helper, EasyNPC<?> easyNPC) {
    Entity entity = easyNPC.getEntity();
    CompoundTag entityTag = entity.saveWithoutId(new CompoundTag());
    entity.setRemoved(Entity.RemovalReason.UNLOADED_TO_CHUNK);

    Entity reloadedEntity = entity.getType().create(helper.getLevel());
    if (reloadedEntity == null) {
      helper.fail("Unable to create " + entity.getType() + " for the reload");
      return null;
    }

    reloadedEntity.load(entityTag);
    if (!helper.getLevel().addFreshEntity(reloadedEntity)
        || !(reloadedEntity instanceof EasyNPC<?> reloadedNPC)) {
      helper.fail("Unable to add the reloaded NPC " + entity.getUUID() + " to the level");
      return null;
    }

    return reloadedNPC;
  }

  private static String getStoredCustomName(UUID entityUUID) {
    return NPCEntityManager.getNPC(entityUUID)
        .map(entry -> entry.npcData().getString(CUSTOM_NAME_TAG))
        .orElse("");
  }

  private static long getStoredHash(UUID entityUUID) {
    return NPCEntityManager.getMetadata(entityUUID).map(NPCEntityMetadata::npcDataHash).orElse(0L);
  }

  private static long getLastUpdate(EasyNPC<?> easyNPC) {
    return easyNPC
        .getEasyNPCStatusData()
        .getStatusDataTimestamp(StatusDataType.NPC_DATA_LAST_UPDATE);
  }
}
