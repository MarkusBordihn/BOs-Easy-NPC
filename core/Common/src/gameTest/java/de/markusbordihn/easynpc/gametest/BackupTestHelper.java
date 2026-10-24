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

import de.markusbordihn.easynpc.api.handler.EasyNPCEntityHandler;
import de.markusbordihn.easynpc.backup.BackupManager;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import de.markusbordihn.easynpc.data.type.ValueType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.NPCEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.handler.OwnerHandler;
import de.markusbordihn.easynpc.handler.PresetHandler;
import de.markusbordihn.easynpc.io.BackgroundFileWriter;
import de.markusbordihn.easynpc.io.BackupDataFiles;
import de.markusbordihn.easynpc.io.PresetFileHandler;
import java.io.File;
import java.nio.file.Path;
import java.time.Duration;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.function.Predicate;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class BackupTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 OWNER_POSITION = new Vec3(2, 2, 1);
  private static final String SPREAD_NAME_PREFIX = "Backup Spread ";
  private static final String RESTORE_NAME_PREFIX = "Backup Restore ";
  private static final int NPC_COUNT = BackupManager.BACKUP_BATCH_SIZE_PER_TICK + 2;
  private static final int LIGHT_LEVEL = 12;
  private static final Duration WRITER_TIMEOUT = Duration.ofSeconds(30);

  private BackupTestHelper() {}

  public static void assertBackupIsSpreadOverTicks(
      GameTestHelper helper, EntityType<?> entityType) {
    List<EasyNPC<?>> spawnedNPCs = spawnNamedNPCs(helper, entityType, SPREAD_NAME_PREFIX);

    BackupManager.startBackupRun();
    GameTestHelpers.assertTrue(
        helper,
        "The backup run must queue every NPC instead of backing it up right away",
        BackupManager.getPendingBackupCount() >= NPC_COUNT);
    GameTestHelpers.assertEquals(
        helper,
        "The backup run must not write any file within the tick that started it",
        0,
        countBackupFiles(spawnedNPCs));

    BackupManager.performBackup();
    GameTestHelpers.assertTrue(
        helper,
        "A single tick must not back up more than "
            + BackupManager.BACKUP_BATCH_SIZE_PER_TICK
            + " NPCs",
        countBackupFiles(spawnedNPCs) < NPC_COUNT);

    drainBackupRun(helper);
    GameTestHelpers.assertEquals(
        helper,
        "Every NPC must have a backup file after the backup run",
        NPC_COUNT,
        countBackupFiles(spawnedNPCs));
  }

  public static void assertBackupRestoresDeletedNPCs(
      GameTestHelper helper, EntityType<?> entityType) {
    List<EasyNPC<?>> spawnedNPCs = spawnNamedNPCs(helper, entityType, RESTORE_NAME_PREFIX);
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, OWNER_POSITION, "backup-restore-player");
    UUID ownerUUID = serverPlayer.getUUID();
    for (EasyNPC<?> easyNPC : spawnedNPCs) {
      GameTestHelpers.assertTrue(
          helper,
          "Failed to set the owner of " + easyNPC.getEntityUUID(),
          OwnerHandler.setOwner(easyNPC, serverPlayer));
    }

    BackupManager.startBackupRun();
    drainBackupRun(helper);

    List<String> npcNames = new ArrayList<>(NPC_COUNT);
    List<UUID> npcUUIDs = new ArrayList<>(NPC_COUNT);
    List<Vec3> npcPositions = new ArrayList<>(NPC_COUNT);
    List<File> backupFiles = new ArrayList<>(NPC_COUNT);
    for (EasyNPC<?> easyNPC : spawnedNPCs) {
      File backupFile = getBackupFile(easyNPC.getEntityUUID());
      GameTestHelpers.assertTrue(
          helper,
          "The backup file for " + easyNPC.getEntityUUID() + " is missing",
          backupFile != null && backupFile.isFile());
      npcNames.add(easyNPC.getEntity().getCustomName().getString());
      npcUUIDs.add(easyNPC.getEntityUUID());
      npcPositions.add(easyNPC.getEntity().position());
      backupFiles.add(backupFile);
      EasyNPCEntityHandler.delete(easyNPC);
    }

    for (String npcName : npcNames) {
      GameTestHelpers.assertEquals(
          helper,
          "The deleted NPC " + npcName + " is still present",
          0,
          countNPCsWithName(npcName));
    }

    for (File backupFile : backupFiles) {
      CompoundTag backupTag = PresetFileHandler.load(backupFile);
      GameTestHelpers.assertNotNull(
          helper, "The backup file " + backupFile + " could not be read", backupTag);
      GameTestHelpers.assertTrue(
          helper,
          "The backup file " + backupFile + " could not be restored",
          PresetHandler.importPreset(helper.getLevel(), backupTag));
    }

    for (int i = 0; i < npcNames.size(); i++) {
      String npcName = npcNames.get(i);
      EasyNPC<?> restoredNPC = findNPCWithName(npcName);
      GameTestHelpers.assertNotNull(
          helper, "The NPC " + npcName + " was not restored from its backup", restoredNPC);
      GameTestHelpers.assertEquals(
          helper,
          "The restored NPC " + npcName + " lost its configuration",
          LIGHT_LEVEL,
          ((DisplayAttributeDataCapable<?>) restoredNPC)
              .getDisplayIntAttribute(DisplayAttributeType.LIGHT_LEVEL));
      GameTestHelpers.assertEquals(
          helper,
          "The restored NPC " + npcName + " lost its UUID",
          npcUUIDs.get(i),
          restoredNPC.getEntityUUID());
      GameTestHelpers.assertEquals(
          helper,
          "The restored NPC " + npcName + " lost its position",
          npcPositions.get(i),
          restoredNPC.getEntity().position());
      GameTestHelpers.assertEquals(
          helper,
          "The restored NPC " + npcName + " lost its owner",
          ownerUUID,
          ((OwnerDataCapable<?>) restoredNPC).getOwnerUUID());
    }
  }

  public static void assertBackupMatchesExport(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, OWNER_POSITION, "backup-export-player");
    GameTestHelpers.assertTrue(
        helper, "Failed to set the owner", OwnerHandler.setOwner(easyNPC, serverPlayer));

    BackupManager.startBackupRun();
    drainBackupRun(helper);

    CompoundTag expectedBackup = PresetHandler.prepareExportData(easyNPC);
    expectedBackup.putUUID(OwnerDataCapable.DATA_OWNER_TAG, serverPlayer.getUUID());
    GameTestHelpers.assertEquals(
        helper,
        "The backup must match the preset export of the NPC",
        withoutVolatilePresetData(expectedBackup),
        withoutVolatilePresetData(PresetFileHandler.load(getBackupFile(easyNPC.getEntityUUID()))));
  }

  public static void assertUnchangedNPCIsLinkedIntoNextDay(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    UUID entityUUID = easyNPC.getEntityUUID();
    LocalDate today = LocalDate.now();

    BackupManager.reset();
    BackupManager.startBackupRun(toDate(today.minusDays(1)));
    drainBackupRun(helper);
    File previousBackupFile = getBackupFile(entityUUID);
    CompoundTag previousBackup = PresetFileHandler.load(previousBackupFile);
    GameTestHelpers.assertNotNull(
        helper, "The backup of the previous day is missing", previousBackup);

    BackupManager.startBackupRun(toDate(today));
    drainBackupRun(helper);
    GameTestHelpers.assertTrue(
        helper,
        "An unchanged NPC must be linked into the new day instead of exported again",
        BackupManager.getLinkedBackupCount() > 0);
    File backupFile = getBackupFile(entityUUID);
    GameTestHelpers.assertEquals(
        helper,
        "The linked backup must match the backup of the previous day",
        previousBackup,
        PresetFileHandler.load(backupFile));

    BackupDataFiles.removeBackupFoldersBefore(today);
    GameTestHelpers.assertTrue(
        helper,
        "Removing the previous day must remove its backup",
        !previousBackupFile.exists());
    CompoundTag backupTag = PresetFileHandler.load(backupFile);
    GameTestHelpers.assertEquals(
        helper,
        "The linked backup must stay readable after the previous day was removed",
        previousBackup,
        backupTag);

    EasyNPCEntityHandler.delete(easyNPC);
    GameTestHelpers.assertTrue(
        helper,
        "The linked backup could not be restored",
        PresetHandler.importPreset(helper.getLevel(), backupTag));
    GameTestHelpers.assertNotNull(
        helper,
        "The NPC was not restored from the linked backup",
        LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel()));
  }

  public static void assertChangedNPCIsSavedAndExported(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    UUID entityUUID = easyNPC.getEntityUUID();
    BackupManager.startBackupRun();
    drainBackupRun(helper);

    ((DisplayAttributeDataCapable<?>) easyNPC)
        .setDisplayAttribute(DisplayAttributeType.LIGHT_LEVEL, ValueType.INTEGER, LIGHT_LEVEL + 1);
    BackupManager.startBackupRun();
    drainBackupRun(helper);

    GameTestHelpers.assertTrue(
        helper,
        "A changed NPC must be exported again",
        BackupManager.getExportedBackupCount() > 0);
    GameTestHelpers.assertTrue(
        helper,
        "The backup run must store the change of the NPC",
        !easyNPC.getEasyNPCStatusData().hasUnsavedNPCData());
    GameTestHelpers.assertTrue(
        helper,
        "The backup run must write the NPC file together with the backup",
        !NPCEntityManager.isNPCFileDirty(entityUUID));
    CompoundTag backupTag = PresetFileHandler.load(getBackupFile(entityUUID));
    GameTestHelpers.assertNotNull(helper, "The backup of the changed NPC is missing", backupTag);
    GameTestHelpers.assertEquals(
        helper,
        "The backup must contain the change of the NPC",
        withoutVolatilePresetData(PresetHandler.prepareExportData(easyNPC)),
        withoutVolatilePresetData(backupTag));
  }

  public static void assertBackupRestoresOverChangedNPC(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    UUID entityUUID = easyNPC.getEntityUUID();
    NPCStorageWriteTestHelper.applyNPCDataChange(
        easyNPC, NPCStorageWriteTestHelper.FIRST_NPC_DATA_CHANGE);
    BackupManager.startBackupRun();
    drainBackupRun(helper);
    CompoundTag backupTag = PresetFileHandler.load(getBackupFile(entityUUID));
    GameTestHelpers.assertNotNull(helper, "The backup of the NPC is missing", backupTag);

    NPCStorageWriteTestHelper.applyNPCDataChange(
        easyNPC, NPCStorageWriteTestHelper.SECOND_NPC_DATA_CHANGE);
    GameTestHelpers.assertTrue(
        helper,
        "The backup could not be restored over the changed NPC",
        PresetHandler.importPreset(helper.getLevel(), backupTag));
    GameTestHelpers.assertEquals(
        helper,
        "Restoring a backup must replace the changed NPC instead of adding a second one",
        1L,
        LivingEntityManager.getServerEasyNPCEntities()
            .filter(serverNPC -> entityUUID.equals(serverNPC.getEntityUUID()))
            .count());
    EasyNPC<?> restoredNPC =
        LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel());
    GameTestHelpers.assertNotNull(helper, "The restored NPC is missing", restoredNPC);
    NPCStorageWriteTestHelper.assertNPCDataChange(
        helper,
        restoredNPC,
        NPCStorageWriteTestHelper.FIRST_NPC_DATA_CHANGE,
        "The NPC restored from its backup");

    GameTestHelpers.assertTrue(
        helper,
        "The restored NPC was not despawned",
        EasyNPCEntityHandler.despawn(restoredNPC, NPCRemovalReason.DESPAWNED));
    GameTestHelpers.assertTrue(
        helper,
        "The restored NPC was not spawned from its stored data",
        EasyNPCEntityHandler.spawn(
            entityUUID, helper.getLevel(), helper.absoluteVec(NPC_POSITION)));
    EasyNPC<?> spawnedNPC =
        LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel());
    GameTestHelpers.assertNotNull(
        helper, "The restored NPC spawned from its stored data is missing", spawnedNPC);
    NPCStorageWriteTestHelper.assertNPCDataChange(
        helper,
        spawnedNPC,
        NPCStorageWriteTestHelper.FIRST_NPC_DATA_CHANGE,
        "Spawning the restored NPC from its stored data");
  }

  public static void assertBackupRestoresFullConfiguration(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    UUID entityUUID = easyNPC.getEntityUUID();
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, OWNER_POSITION, "backup-full-restore-player");
    GameTestHelpers.assertTrue(
        helper, "Failed to set the owner", OwnerHandler.setOwner(easyNPC, serverPlayer));
    NPCStorageWriteTestHelper.applyNPCDataChange(
        easyNPC, NPCStorageWriteTestHelper.FIRST_NPC_DATA_CHANGE);

    BackupManager.startBackupRun();
    drainBackupRun(helper);
    CompoundTag expectedExport = withoutVolatilePresetData(PresetHandler.prepareExportData(easyNPC));
    CompoundTag backupTag = PresetFileHandler.load(getBackupFile(entityUUID));
    GameTestHelpers.assertNotNull(helper, "The backup of the NPC is missing", backupTag);

    EasyNPCEntityHandler.delete(easyNPC);
    GameTestHelpers.assertTrue(
        helper,
        "The deleted NPC is still present",
        LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel()) == null);
    GameTestHelpers.assertTrue(
        helper,
        "The backup of the deleted NPC could not be restored",
        PresetHandler.importPreset(helper.getLevel(), backupTag));

    EasyNPC<?> restoredNPC =
        LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel());
    GameTestHelpers.assertNotNull(helper, "The NPC was not restored from its backup", restoredNPC);
    NPCStorageWriteTestHelper.assertNPCDataChange(
        helper,
        restoredNPC,
        NPCStorageWriteTestHelper.FIRST_NPC_DATA_CHANGE,
        "The NPC restored from its backup");
    GameTestHelpers.assertEquals(
        helper,
        "The restored NPC lost its owner",
        serverPlayer.getUUID(),
        ((OwnerDataCapable<?>) restoredNPC).getOwnerUUID());
    GameTestHelpers.assertEquals(
        helper,
        "The restored NPC must have the configuration it had before it was deleted",
        expectedExport,
        withoutVolatilePresetData(PresetHandler.prepareExportData(restoredNPC)));
  }

  private static CompoundTag withoutVolatilePresetData(CompoundTag presetTag) {
    CompoundTag normalizedTag = presetTag.copy();
    normalizedTag.remove(PresetDataCapable.PRESET_UUID_TAG);
    if (normalizedTag.contains(PresetDataCapable.PRESET_METADATA_TAG)) {
      CompoundTag metadataTag = normalizedTag.getCompound(PresetDataCapable.PRESET_METADATA_TAG);
      metadataTag.remove(PresetMetadata.TAG_CREATED);
      metadataTag.remove(PresetMetadata.TAG_MODIFIED);
    }
    return normalizedTag;
  }

  private static Date toDate(LocalDate localDate) {
    return Date.from(localDate.atStartOfDay(ZoneId.systemDefault()).toInstant());
  }

  private static List<EasyNPC<?>> spawnNamedNPCs(
      GameTestHelper helper, EntityType<?> entityType, String namePrefix) {
    List<EasyNPC<?>> spawnedNPCs = new ArrayList<>(NPC_COUNT);
    for (int i = 0; i < NPC_COUNT; i++) {
      EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
      easyNPC.getEntity().setCustomName(Component.literal(namePrefix + easyNPC.getEntityUUID()));
      ((DisplayAttributeDataCapable<?>) easyNPC)
          .setDisplayAttribute(DisplayAttributeType.LIGHT_LEVEL, ValueType.INTEGER, LIGHT_LEVEL);
      spawnedNPCs.add(easyNPC);
    }
    return spawnedNPCs;
  }

  static void drainBackupRun(GameTestHelper helper) {
    int pendingBackups = BackupManager.getPendingBackupCount();
    while (pendingBackups > 0) {
      BackupManager.performBackup();

      int remainingBackups = BackupManager.getPendingBackupCount();
      GameTestHelpers.assertTrue(
          helper,
          "The backup run stopped with " + remainingBackups + " NPCs left in the queue",
          remainingBackups < pendingBackups);
      pendingBackups = remainingBackups;
    }

    GameTestHelpers.assertTrue(
        helper,
        "The backup files were not written in time",
        BackgroundFileWriter.awaitPendingWrites(WRITER_TIMEOUT));
  }

  static File getBackupFile(UUID entityUUID) {
    Date backupRunDate = BackupManager.getBackupRunDate();
    if (backupRunDate == null) {
      return null;
    }

    Path backupFilePath = BackupDataFiles.getBackupFile(entityUUID, backupRunDate);
    return backupFilePath != null ? backupFilePath.toFile() : null;
  }

  private static int countBackupFiles(List<EasyNPC<?>> easyNPCs) {
    int backupFileCount = 0;
    for (EasyNPC<?> easyNPC : easyNPCs) {
      File backupFile = getBackupFile(easyNPC.getEntityUUID());
      if (backupFile != null && backupFile.isFile()) {
        backupFileCount++;
      }
    }
    return backupFileCount;
  }

  private static int countNPCsWithName(String npcName) {
    return (int) LivingEntityManager.getServerEasyNPCEntities().filter(hasName(npcName)).count();
  }

  private static EasyNPC<?> findNPCWithName(String npcName) {
    return LivingEntityManager.getServerEasyNPCEntities()
        .filter(hasName(npcName))
        .findFirst()
        .orElse(null);
  }

  private static Predicate<EasyNPC<?>> hasName(String npcName) {
    return easyNPC ->
        easyNPC.getEntity() != null
            && easyNPC.getEntity().getCustomName() != null
            && npcName.equals(easyNPC.getEntity().getCustomName().getString());
  }
}
