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
import de.markusbordihn.easynpc.data.type.ValueType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.handler.OwnerHandler;
import de.markusbordihn.easynpc.handler.PresetHandler;
import de.markusbordihn.easynpc.io.BackupDataFiles;
import de.markusbordihn.easynpc.io.PresetFileHandler;
import java.io.File;
import java.nio.file.Path;
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

  private static void drainBackupRun(GameTestHelper helper) {
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
  }

  private static File getBackupFile(UUID entityUUID) {
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
