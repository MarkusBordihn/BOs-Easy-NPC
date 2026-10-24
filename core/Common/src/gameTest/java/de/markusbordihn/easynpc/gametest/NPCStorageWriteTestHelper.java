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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.api.handler.EasyNPCEntityHandler;
import de.markusbordihn.easynpc.api.handler.EasyNPCShoulderHandler;
import de.markusbordihn.easynpc.backup.BackupManager;
import de.markusbordihn.easynpc.data.attribute.InventoryAttributeType;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.data.npc.NPCEntityMetadata;
import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import de.markusbordihn.easynpc.data.npc.ShoulderSide;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.data.saveddata.NPCEntityData;
import de.markusbordihn.easynpc.data.status.StatusDataType;
import de.markusbordihn.easynpc.data.storage.NPCStorageStatistics;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.data.type.ValueType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.NPCEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import de.markusbordihn.easynpc.handler.EquipmentHandler;
import de.markusbordihn.easynpc.handler.NameHandler;
import de.markusbordihn.easynpc.handler.OwnerHandler;
import de.markusbordihn.easynpc.handler.PauseManager;
import de.markusbordihn.easynpc.handler.PresetHandler;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class NPCStorageWriteTestHelper {

  public static final int MASS_NPC_COUNT = 200;

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[NPC Storage Write Test]";

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 PLAYER_POSITION = new Vec3(2, 2, 1);
  private static final int AI_TICKS_BEFORE_UNLOAD = 60;
  private static final int TICKS_BETWEEN_BACKUP_RUNS = 5;
  private static final long BACKUP_TIMESTAMP_OFFSET_MILLISECONDS = 60L * 60L * 1000L;
  private static final int LIGHT_LEVEL = 12;
  private static final String SHOULDER_NPC_NAME = "Shoulder Storage NPC";
  private static final String DIALOG_NAME = "Storage Dialog";
  private static final String UNMARKED_CHANGE_TAG = "easy_npc_storage_test";
  private static final String CUSTOM_NAME_TAG = "CustomName";
  static final NPCDataChange FIRST_NPC_DATA_CHANGE =
      new NPCDataChange(
          "First Changed NPC",
          LIGHT_LEVEL,
          "First Changed Dialog",
          Items.DIAMOND_HELMET,
          Items.EMERALD,
          Items.EMERALD,
          true);
  static final NPCDataChange SECOND_NPC_DATA_CHANGE =
      new NPCDataChange(
          "Second Changed NPC",
          5,
          "Second Changed Dialog",
          Items.IRON_HELMET,
          Items.GOLD_INGOT,
          Items.GOLD_INGOT,
          false);

  private NPCStorageWriteTestHelper() {}

  public static void assertUnchangedReloadWritesNoFile(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    assertUnchangedReloadWritesNoFile(helper, easyNPC);
  }

  public static void assertUnchangedReloadWithObjectivesWritesNoFile(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, "storage-objective-owner");
    ObjectiveDataCapable<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    objectiveData.addOrUpdateCustomObjective(new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER));
    OwnerHandler.setOwner(easyNPC, serverPlayer);
    objectiveData.refreshCustomObjectives();
    assertUnchangedReloadWritesNoFile(helper, easyNPC);
  }

  public static void assertUnchangedReloadWithTradingWritesNoFile(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    TradingDataCapable<?> tradingData = easyNPC.getEasyNPCTradingData();
    MerchantOffers offers = new MerchantOffers();
    offers.add(
        new MerchantOffer(new ItemStack(Items.DIAMOND), new ItemStack(Items.EMERALD), 1, 0, 1.0f));
    tradingData.getTradingDataSet().setType(TradingType.BASIC);
    tradingData.setTradingOffers(offers);
    assertUnchangedReloadWritesNoFile(helper, easyNPC);
  }

  private static void assertUnchangedReloadWritesNoFile(
      GameTestHelper helper, EasyNPC<?> easyNPC) {
    UUID entityUUID = easyNPC.getEntityUUID();
    saveAndFlush(easyNPC);
    long savedLastUpdate = getLastUpdate(easyNPC);
    EasyNPC<?> loadedNPC = reloadFromChunk(helper, easyNPC);
    if (loadedNPC == null) {
      return;
    }

    GameTestHelpers.assertTrue(
        helper,
        "Loading an NPC from its chunk must keep its last update timestamp",
        getLastUpdate(loadedNPC) == savedLastUpdate);
    helper.runAfterDelay(
        AI_TICKS_BEFORE_UNLOAD,
        () -> {
          GameTestHelpers.assertTrue(
              helper,
              "Running the AI of an unchanged NPC must not change its last update timestamp",
              getLastUpdate(loadedNPC) == savedLastUpdate);
          GameTestHelpers.assertTrue(
              helper,
              "Loading an NPC from its chunk must not mark it as changed",
              !loadedNPC.getEasyNPCStatusData().hasUnsavedNPCData());

          long skippedUnloadsBefore = getStatistics().skippedUnchangedUnloads();
          reloadFromChunk(helper, loadedNPC);
          GameTestHelpers.assertTrue(
              helper,
              "Unloading an unchanged NPC must not queue a write of its NPC file",
              !NPCEntityManager.isNPCFileDirty(entityUUID));
          GameTestHelpers.assertTrue(
              helper,
              "Unloading an unchanged NPC must be counted as skipped",
              getStatistics().skippedUnchangedUnloads() > skippedUnloadsBefore);
          helper.succeed();
        });
  }

  public static void assertEquipmentChangeIsSaved(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    EquipmentHandler.setEquipmentSlotItem(
        easyNPC, EquipmentSlot.HEAD, new ItemStack(Items.DIAMOND_HELMET));
    assertChangeIsWrittenOnUnload(helper, easyNPC, "An equipment change");
  }

  public static void assertCustomNameChangeIsSaved(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    easyNPC.getEntity().setCustomName(Component.literal("Renamed Storage NPC"));
    assertChangeIsWrittenOnUnload(helper, easyNPC, "A custom name change");
  }

  public static void assertCustomNameRemovalIsSaved(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    easyNPC.getEntity().setCustomName(Component.literal("Named Storage NPC"));
    saveAndFlush(easyNPC);
    assertUnchanged(helper, easyNPC, "A freshly saved NPC");

    NameHandler.setCustomName(easyNPC, "", -1, false);
    assertChangeIsWrittenOnUnload(helper, easyNPC, "Removing the custom name");
  }

  public static void assertInventoryChangeIsSaved(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    easyNPC.getEasyNPCInventoryData().getEasyNPCInventory().addItem(new ItemStack(Items.EMERALD));
    assertChangeIsWrittenOnUnload(helper, easyNPC, "An inventory change");
  }

  public static void assertItemPickupIsSaved(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    AttributeHandler.setInventoryAttribute(easyNPC, InventoryAttributeType.PICK_UP_ITEMS, true);
    saveAndFlush(easyNPC);
    assertUnchanged(helper, easyNPC, "A freshly saved NPC");

    Mob mob = easyNPC.getMob();
    ItemEntity itemEntity =
        new ItemEntity(
            helper.getLevel(), mob.getX(), mob.getY(), mob.getZ(), new ItemStack(Items.EMERALD));
    itemEntity.setDeltaMovement(Vec3.ZERO);
    helper.getLevel().addFreshEntity(itemEntity);
    mob.aiStep();
    GameTestHelpers.assertTrue(
        helper, "The NPC must have picked up the item", itemEntity.isRemoved());
    assertChangeIsWrittenOnUnload(helper, easyNPC, "Picking up an item");
  }

  public static void assertPresetImportIsSaved(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    CompoundTag presetTag = easyNPC.getEasyNPCPresetData().serializePresetData();
    presetTag.putUUID(Entity.UUID_TAG, easyNPC.getEntityUUID());
    GameTestHelpers.assertTrue(
        helper,
        "The preset import failed",
        PresetHandler.importPreset(helper.getLevel(), presetTag));
    assertChangeIsWrittenOnUnload(helper, easyNPC, "A preset import");
  }

  public static void assertPresetImportWithTypeChangeIsSaved(
      GameTestHelper helper, EntityType<?> entityType, EntityType<?> otherEntityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    UUID entityUUID = easyNPC.getEntityUUID();
    String otherEntityTypeId = EntityType.getKey(otherEntityType).toString();
    CompoundTag presetTag = easyNPC.getEasyNPCPresetData().serializePresetData();
    presetTag.putUUID(Entity.UUID_TAG, entityUUID);
    presetTag.putString(Entity.ID_TAG, otherEntityTypeId);
    GameTestHelpers.assertTrue(
        helper,
        "The preset import failed",
        PresetHandler.importPreset(helper.getLevel(), presetTag));

    EasyNPC<?> importedNPC =
        LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel());
    GameTestHelpers.assertNotNull(helper, "The imported NPC is missing", importedNPC);
    GameTestHelpers.assertEquals(
        helper,
        "The imported NPC must have the new entity type",
        otherEntityTypeId,
        importedNPC.getEntityTypeId());
    GameTestHelpers.assertTrue(
        helper,
        "A preset import with a type change must mark the new NPC as changed",
        importedNPC.getEasyNPCStatusData().hasUnsavedNPCData());

    NPCEntityManager.saveAllDirtyNPCs();
    GameTestHelpers.assertTrue(
        helper,
        "A preset import with a type change must queue a write of the NPC file",
        NPCEntityManager.isNPCFileDirty(entityUUID));
    GameTestHelpers.assertEquals(
        helper,
        "The stored metadata must have the new entity type",
        otherEntityTypeId,
        NPCEntityManager.getMetadata(entityUUID).map(NPCEntityMetadata::entityType).orElse(""));
  }

  public static void assertLoadOnRunningNPCIsSaved(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    easyNPC.getEntity().load(easyNPC.getEntity().saveWithoutId(new CompoundTag()));
    assertChangeIsWrittenOnUnload(helper, easyNPC, "Loading data into a running NPC");
  }

  public static void assertRemovalIsStillSaved(
      GameTestHelper helper,
      EntityType<?> entityType,
      Entity.RemovalReason removalReason,
      NPCRemovalReason expectedRemovalReason) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    UUID entityUUID = easyNPC.getEntityUUID();
    long serializationsBefore = getStatistics().serializations();

    easyNPC.getEntity().setRemoved(removalReason);
    GameTestHelpers.assertTrue(
        helper,
        "A removal with reason " + removalReason + " must still serialize the NPC",
        getStatistics().serializations() > serializationsBefore);
    GameTestHelpers.assertEquals(
        helper,
        "The stored removal reason must match " + removalReason,
        expectedRemovalReason,
        NPCEntityManager.getMetadata(entityUUID)
            .map(NPCEntityMetadata::removalReason)
            .orElse(NPCRemovalReason.NONE));
  }

  public static void assertShoulderRoundTripWritesNoUnchangedFile(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    UUID entityUUID = easyNPC.getEntityUUID();
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, "storage-shoulder-player");
    serverPlayer.setOnGround(true);
    easyNPC.getEntity().setCustomName(Component.literal(SHOULDER_NPC_NAME));

    GameTestHelpers.assertTrue(
        helper,
        "The NPC did not sit on the shoulder",
        EasyNPCShoulderHandler.sitOnShoulder(easyNPC, serverPlayer));
    GameTestHelpers.assertTrue(
        helper,
        "Sitting on a shoulder with unsaved changes must queue a write of the NPC file",
        NPCEntityManager.isNPCFileDirty(entityUUID));
    NPCEntityData.get().saveAllDirtyNPCs();

    GameTestHelpers.assertTrue(
        helper,
        "The NPC was not dismounted from the shoulder",
        EasyNPCShoulderHandler.dismount(serverPlayer, ShoulderSide.LEFT));
    EasyNPC<?> dismountedNPC =
        LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel());
    GameTestHelpers.assertNotNull(helper, "The dismounted NPC is missing", dismountedNPC);
    GameTestHelpers.assertEquals(
        helper,
        "The dismounted NPC must keep the change made before sitting on the shoulder",
        SHOULDER_NPC_NAME,
        dismountedNPC.getEntity().getName().getString());
    assertUnchanged(helper, dismountedNPC, "Dismounting from a shoulder");

    long skippedUnloadsBefore = getStatistics().skippedUnchangedUnloads();
    dismountedNPC.getEntity().setRemoved(Entity.RemovalReason.UNLOADED_TO_CHUNK);
    GameTestHelpers.assertTrue(
        helper,
        "Unloading an unchanged dismounted NPC must not queue a write of its NPC file",
        !NPCEntityManager.isNPCFileDirty(entityUUID));
    GameTestHelpers.assertTrue(
        helper,
        "Unloading an unchanged dismounted NPC must be counted as skipped",
        getStatistics().skippedUnchangedUnloads() > skippedUnloadsBefore);
    GameTestHelpers.assertEquals(
        helper,
        "The unloaded NPC must no longer be stored as sitting on a shoulder",
        NPCRemovalReason.UNLOADED_TO_CHUNK,
        NPCEntityManager.getMetadata(entityUUID)
            .map(NPCEntityMetadata::removalReason)
            .orElse(NPCRemovalReason.NONE));
  }

  public static void assertShoulderRoundTripsKeepLatestChanges(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    UUID entityUUID = easyNPC.getEntityUUID();
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, "storage-shoulder-data-player");
    serverPlayer.setOnGround(true);

    applyNPCDataChange(easyNPC, FIRST_NPC_DATA_CHANGE);
    EasyNPC<?> firstDismountedNPC =
        sitOnShoulderAndDismount(helper, easyNPC, serverPlayer, FIRST_NPC_DATA_CHANGE);
    assertNPCDataChange(
        helper, firstDismountedNPC, FIRST_NPC_DATA_CHANGE, "The first shoulder round trip");

    applyNPCDataChange(firstDismountedNPC, SECOND_NPC_DATA_CHANGE);
    EasyNPC<?> secondDismountedNPC =
        sitOnShoulderAndDismount(helper, firstDismountedNPC, serverPlayer, SECOND_NPC_DATA_CHANGE);
    assertNPCDataChange(
        helper, secondDismountedNPC, SECOND_NPC_DATA_CHANGE, "The second shoulder round trip");

    EasyNPC<?> reloadedNPC = reloadFromChunk(helper, secondDismountedNPC);
    if (reloadedNPC == null) {
      return;
    }

    assertNPCDataChange(
        helper, reloadedNPC, SECOND_NPC_DATA_CHANGE, "A chunk reload after the round trips");
    GameTestHelpers.assertTrue(
        helper,
        "The NPC was not despawned",
        EasyNPCEntityHandler.despawn(reloadedNPC, NPCRemovalReason.DESPAWNED));
    GameTestHelpers.assertTrue(
        helper,
        "The NPC was not spawned from its stored data",
        EasyNPCEntityHandler.spawn(
            entityUUID, helper.getLevel(), helper.absoluteVec(NPC_POSITION)));
    EasyNPC<?> spawnedNPC =
        LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel());
    GameTestHelpers.assertNotNull(
        helper, "The NPC spawned from its stored data is missing", spawnedNPC);
    assertNPCDataChange(helper, spawnedNPC, SECOND_NPC_DATA_CHANGE, "Spawning from the stored data");
  }

  public static void assertBackupRunSkipsUnchangedNPC(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    BackupManager.startBackupRun();
    BackupTestHelper.drainBackupRun(helper);
    File backupFile = BackupTestHelper.getBackupFile(easyNPC.getEntityUUID());
    GameTestHelpers.assertTrue(
        helper,
        "The first backup run must export the NPC",
        backupFile != null && backupFile.isFile());

    long markerTimestamp =
        setBackupFileTimestamp(
            helper, backupFile, System.currentTimeMillis() + BACKUP_TIMESTAMP_OFFSET_MILLISECONDS);
    BackupManager.reset();
    BackupManager.startBackupRun();
    BackupTestHelper.drainBackupRun(helper);
    GameTestHelpers.assertEquals(
        helper,
        "A backup run after a restart must not export an NPC that did not change",
        markerTimestamp,
        backupFile.lastModified());
  }

  public static void assertBackupRunExportsChangedNPC(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    BackupManager.startBackupRun();
    BackupTestHelper.drainBackupRun(helper);
    File backupFile = BackupTestHelper.getBackupFile(easyNPC.getEntityUUID());
    GameTestHelpers.assertTrue(
        helper,
        "The first backup run must export the NPC",
        backupFile != null && backupFile.isFile());

    long markerTimestamp =
        setBackupFileTimestamp(
            helper, backupFile, System.currentTimeMillis() - BACKUP_TIMESTAMP_OFFSET_MILLISECONDS);
    helper.runAfterDelay(
        TICKS_BETWEEN_BACKUP_RUNS,
        () -> {
          ((DisplayAttributeDataCapable<?>) easyNPC)
              .setDisplayAttribute(
                  DisplayAttributeType.LIGHT_LEVEL, ValueType.INTEGER, LIGHT_LEVEL);
          BackupManager.startBackupRun();
          BackupTestHelper.drainBackupRun(helper);
          GameTestHelpers.assertTrue(
              helper,
              "A backup run must export an NPC that changed since its last backup",
              backupFile.lastModified() != markerTimestamp);
          helper.succeed();
        });
  }

  public static void assertListCommandReadsNoNPCFile(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    easyNPC.getEntity().setRemoved(Entity.RemovalReason.UNLOADED_TO_CHUNK);

    long filesReadBefore = getStatistics().filesRead();
    int result =
        GameTestHelpers.executeCommand(
            helper,
            GameTestHelpers.consoleCommandSource(helper, NPC_POSITION),
            Constants.MOD_COMMAND + " list");
    GameTestHelpers.assertTrue(helper, "The list command failed", result > 0);
    GameTestHelpers.assertEquals(
        helper,
        "Listing NPCs must not read any NPC file",
        filesReadBefore,
        getStatistics().filesRead());
  }

  public static void assertRepeatedSaveKeepsIndexClean(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    NPCEntityData npcEntityData = NPCEntityData.get();
    boolean wasIndexDirty = npcEntityData.isDirty();
    npcEntityData.setDirty(false);

    NPCEntityManager.saveNPC(easyNPC);
    boolean isIndexDirty = npcEntityData.isDirty();
    if (wasIndexDirty) {
      npcEntityData.setDirty();
    }
    GameTestHelpers.assertTrue(
        helper,
        "Saving an unchanged NPC again must not mark the index as changed",
        !isIndexDirty);
    GameTestHelpers.assertTrue(
        helper,
        "Saving an unchanged NPC again must not queue a write of its NPC file",
        !NPCEntityManager.isNPCFileDirty(easyNPC.getEntityUUID()));
  }

  public static void assertUnchangedPauseStateKeepsNPCClean(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    PauseManager.setPaused(easyNPC, PauseManager.isPaused(easyNPC));
    assertUnchanged(helper, easyNPC, "Setting the same pause state");
  }

  public static void assertDialogChangeIsSaved(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    DialogDataEntry dialogEntry = new DialogDataEntry(DIALOG_NAME);
    easyNPC.getEasyNPCDialogData().setDialog(dialogEntry.getId(), dialogEntry);
    assertChangeIsWrittenOnUnload(helper, easyNPC, "Adding a dialog");
  }

  public static void assertDialogRemovalIsSaved(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    DialogDataCapable<?> dialogData = easyNPC.getEasyNPCDialogData();
    DialogDataEntry dialogEntry = new DialogDataEntry(DIALOG_NAME);
    UUID dialogId = dialogEntry.getId();
    dialogData.setDialog(dialogId, dialogEntry);
    saveAndFlush(easyNPC);
    assertUnchanged(helper, easyNPC, "A freshly saved NPC");

    GameTestHelpers.assertTrue(
        helper,
        "Removing a missing dialog must report that nothing was removed",
        !dialogData.removeDialog(UUID.randomUUID()));
    assertUnchanged(helper, easyNPC, "Removing a missing dialog");

    GameTestHelpers.assertTrue(
        helper, "The dialog was not removed", dialogData.removeDialog(dialogId));
    assertChangeIsWrittenOnUnload(helper, easyNPC, "Removing a dialog");
  }

  public static void assertObjectiveChangeIsSaved(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    easyNPC
        .getEasyNPCObjectiveData()
        .addOrUpdateCustomObjective(new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER));
    assertChangeIsWrittenOnUnload(helper, easyNPC, "Adding an objective");
  }

  public static void assertObjectiveRemovalIsSaved(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ObjectiveDataCapable<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    objectiveData.addOrUpdateCustomObjective(new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER));
    saveAndFlush(easyNPC);
    assertUnchanged(helper, easyNPC, "A freshly saved NPC");

    GameTestHelpers.assertTrue(
        helper,
        "The objective was not removed",
        objectiveData.removeCustomObjective(ObjectiveType.FOLLOW_OWNER));
    assertChangeIsWrittenOnUnload(helper, easyNPC, "Removing an objective");
  }

  public static void assertObjectiveRefreshKeepsNPCClean(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ObjectiveDataCapable<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    objectiveData.addOrUpdateCustomObjective(new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER));
    saveAndFlush(easyNPC);
    assertUnchanged(helper, easyNPC, "A freshly saved NPC");

    objectiveData.refreshCustomObjectives();
    assertUnchanged(helper, easyNPC, "Refreshing an objective without a valid target");
  }

  public static void assertHourlyFlushKeepsUnchangedNPCClean(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    long savedLastUpdate = getLastUpdate(easyNPC);
    helper.runAfterDelay(
        AI_TICKS_BEFORE_UNLOAD,
        () -> {
          NPCEntityManager.saveAllDirtyNPCs();
          GameTestHelpers.assertTrue(
              helper,
              "The hourly flush must not queue a write of an unchanged NPC file",
              !NPCEntityManager.isNPCFileDirty(easyNPC.getEntityUUID()));
          GameTestHelpers.assertEquals(
              helper,
              "The hourly flush must not change the last update timestamp of an unchanged NPC",
              savedLastUpdate,
              getLastUpdate(easyNPC));
          helper.succeed();
        });
  }

  public static void assertHourlyFlushSavesUnmarkedChange(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    long savedLastUpdate = getLastUpdate(easyNPC);
    helper.runAfterDelay(
        TICKS_BETWEEN_BACKUP_RUNS,
        () -> {
          easyNPC.getEntity().addTag(UNMARKED_CHANGE_TAG);
          NPCEntityManager.saveAllDirtyNPCs();
          GameTestHelpers.assertTrue(
              helper,
              "The hourly flush must queue a write of an NPC with an unmarked change",
              NPCEntityManager.isNPCFileDirty(easyNPC.getEntityUUID()));
          GameTestHelpers.assertTrue(
              helper,
              "The hourly flush must raise the last update timestamp of an unmarked change",
              getLastUpdate(easyNPC) > savedLastUpdate);
          assertUnchanged(helper, easyNPC, "The hourly flush");
          helper.succeed();
        });
  }

  public static void assertBackupRunExportsUnmarkedChange(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnSavedNPC(helper, entityType);
    BackupManager.startBackupRun();
    BackupTestHelper.drainBackupRun(helper);
    File backupFile = BackupTestHelper.getBackupFile(easyNPC.getEntityUUID());
    GameTestHelpers.assertTrue(
        helper,
        "The first backup run must export the NPC",
        backupFile != null && backupFile.isFile());

    long markerTimestamp =
        setBackupFileTimestamp(
            helper, backupFile, System.currentTimeMillis() - BACKUP_TIMESTAMP_OFFSET_MILLISECONDS);
    helper.runAfterDelay(
        TICKS_BETWEEN_BACKUP_RUNS,
        () -> {
          easyNPC.getEntity().addTag(UNMARKED_CHANGE_TAG);
          BackupManager.startBackupRun();
          BackupTestHelper.drainBackupRun(helper);
          GameTestHelpers.assertTrue(
              helper,
              "A backup run must export an NPC with a change no setter marked",
              backupFile.lastModified() != markerTimestamp);
          helper.succeed();
        });
  }

  public static void assertFileStorageStatsCommandRuns(GameTestHelper helper) {
    int result =
        GameTestHelpers.executeCommand(
            helper,
            GameTestHelpers.consoleCommandSource(helper, NPC_POSITION),
            Constants.MOD_COMMAND + " debug stats file_storage");
    GameTestHelpers.assertEquals(
        helper, "The file storage stats command must succeed", 1, result);
  }

  public static void assertMassReloadWritesNoUnchangedNPCFile(
      GameTestHelper helper, EntityType<?> entityType) {
    List<EasyNPC<?>> spawnedNPCs = new ArrayList<>(MASS_NPC_COUNT);
    for (int i = 0; i < MASS_NPC_COUNT; i++) {
      EasyNPC<?> easyNPC = GameTestHelpers.spawnNPCEntityType(helper, entityType);
      easyNPC.getEntity().setPos(helper.absoluteVec(NPC_POSITION));
      NPCEntityManager.saveNPC(easyNPC);
      spawnedNPCs.add(easyNPC);
    }
    NPCEntityData.get().saveAllDirtyNPCs();

    List<EasyNPC<?>> loadedNPCs = new ArrayList<>(MASS_NPC_COUNT);
    for (EasyNPC<?> easyNPC : spawnedNPCs) {
      EasyNPC<?> loadedNPC = reloadFromChunk(helper, easyNPC);
      if (loadedNPC == null) {
        return;
      }
      loadedNPCs.add(loadedNPC);
    }

    helper.runAfterDelay(
        AI_TICKS_BEFORE_UNLOAD,
        () -> {
          NPCStorageStatistics statisticsBefore = getStatistics();
          long startTime = System.currentTimeMillis();
          List<UUID> entityUUIDs = new ArrayList<>(MASS_NPC_COUNT);
          for (EasyNPC<?> loadedNPC : loadedNPCs) {
            entityUUIDs.add(loadedNPC.getEntityUUID());
            reloadFromChunk(helper, loadedNPC);
          }
          NPCEntityManager.saveAllDirtyNPCs();
          long reloadMilliseconds = System.currentTimeMillis() - startTime;
          long dirtyFiles = entityUUIDs.stream().filter(NPCEntityManager::isNPCFileDirty).count();

          NPCEntityData.get().saveAllDirtyNPCs();
          NPCStorageStatistics statisticsAfter = getStatistics();
          log.info(
              "{} Reload of {} unchanged NPCs took {} ms and queued {} NPC file writes, flush wrote"
                  + " {} files in {} ms",
              LOG_PREFIX,
              MASS_NPC_COUNT,
              reloadMilliseconds,
              dirtyFiles,
              statisticsAfter.filesWritten() - statisticsBefore.filesWritten(),
              statisticsAfter.fileWriteMilliseconds() - statisticsBefore.fileWriteMilliseconds());
          GameTestHelpers.assertEquals(
              helper,
              "Reloading " + MASS_NPC_COUNT + " unchanged NPCs must not queue NPC file writes",
              0L,
              dirtyFiles);
          helper.succeed();
        });
  }

  private static EasyNPC<?> spawnSavedNPC(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    saveAndFlush(easyNPC);
    assertUnchanged(helper, easyNPC, "A freshly saved NPC");
    return easyNPC;
  }

  private static void saveAndFlush(EasyNPC<?> easyNPC) {
    NPCEntityManager.saveNPC(easyNPC);
    NPCEntityData.get().saveAllDirtyNPCs();
  }

  private static void assertUnchanged(GameTestHelper helper, EasyNPC<?> easyNPC, String subject) {
    GameTestHelpers.assertTrue(
        helper,
        subject + " must not mark the NPC as changed",
        !easyNPC.getEasyNPCStatusData().hasUnsavedNPCData());
  }

  private static void assertChangeIsWrittenOnUnload(
      GameTestHelper helper, EasyNPC<?> easyNPC, String change) {
    GameTestHelpers.assertTrue(
        helper,
        change + " must mark the NPC as changed",
        easyNPC.getEasyNPCStatusData().hasUnsavedNPCData());

    UUID entityUUID = easyNPC.getEntityUUID();
    easyNPC.getEntity().setRemoved(Entity.RemovalReason.UNLOADED_TO_CHUNK);
    GameTestHelpers.assertTrue(
        helper,
        change + " must queue a write of the NPC file on unload",
        NPCEntityManager.isNPCFileDirty(entityUUID));
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

  static void applyNPCDataChange(EasyNPC<?> easyNPC, NPCDataChange npcDataChange) {
    easyNPC.getEntity().setCustomName(Component.literal(npcDataChange.name()));
    ((DisplayAttributeDataCapable<?>) easyNPC)
        .setDisplayAttribute(
            DisplayAttributeType.LIGHT_LEVEL, ValueType.INTEGER, npcDataChange.lightLevel());
    DialogDataEntry dialogEntry = new DialogDataEntry(npcDataChange.dialogName());
    easyNPC.getEasyNPCDialogData().setDialog(dialogEntry.getId(), dialogEntry);
    EquipmentHandler.setEquipmentSlotItem(
        easyNPC, EquipmentSlot.HEAD, new ItemStack(npcDataChange.helmet()));
    easyNPC
        .getEasyNPCInventoryData()
        .getEasyNPCInventory()
        .addItem(new ItemStack(npcDataChange.inventoryItem()));

    MerchantOffers offers = new MerchantOffers();
    offers.add(
        new MerchantOffer(
            new ItemStack(Items.DIAMOND), new ItemStack(npcDataChange.tradeResult()), 1, 0, 1.0f));
    TradingDataCapable<?> tradingData = easyNPC.getEasyNPCTradingData();
    tradingData.getTradingDataSet().setType(TradingType.BASIC);
    tradingData.setTradingOffers(offers);

    ObjectiveDataCapable<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    if (npcDataChange.followOwner()) {
      objectiveData.addOrUpdateCustomObjective(new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER));
    } else {
      objectiveData.removeCustomObjective(ObjectiveType.FOLLOW_OWNER);
    }
  }

  private static EasyNPC<?> sitOnShoulderAndDismount(
      GameTestHelper helper,
      EasyNPC<?> easyNPC,
      ServerPlayer serverPlayer,
      NPCDataChange npcDataChange) {
    UUID entityUUID = easyNPC.getEntityUUID();
    GameTestHelpers.assertTrue(
        helper,
        "The NPC did not sit on the shoulder",
        EasyNPCShoulderHandler.sitOnShoulder(easyNPC, serverPlayer));
    GameTestHelpers.assertTrue(
        helper,
        "The stored data of an NPC on a shoulder must hold the change made before sitting down",
        NPCEntityManager.getNPC(entityUUID)
            .map(entry -> entry.npcData().getString(CUSTOM_NAME_TAG))
            .orElse("")
            .contains(npcDataChange.name()));
    NPCEntityData.get().saveAllDirtyNPCs();

    GameTestHelpers.assertTrue(
        helper,
        "The NPC was not dismounted from the shoulder",
        EasyNPCShoulderHandler.dismount(serverPlayer, ShoulderSide.LEFT));
    EasyNPC<?> dismountedNPC =
        LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel());
    GameTestHelpers.assertNotNull(helper, "The dismounted NPC is missing", dismountedNPC);
    return dismountedNPC;
  }

  static void assertNPCDataChange(
      GameTestHelper helper, EasyNPC<?> easyNPC, NPCDataChange npcDataChange, String subject) {
    String message = subject + " must keep the latest ";
    GameTestHelpers.assertEquals(
        helper, message + "name", npcDataChange.name(), easyNPC.getEntity().getName().getString());
    GameTestHelpers.assertEquals(
        helper,
        message + "light level",
        npcDataChange.lightLevel(),
        ((DisplayAttributeDataCapable<?>) easyNPC)
            .getDisplayIntAttribute(DisplayAttributeType.LIGHT_LEVEL));
    GameTestHelpers.assertTrue(
        helper,
        message + "dialog",
        easyNPC
            .getEasyNPCDialogData()
            .hasDialog(new DialogDataEntry(npcDataChange.dialogName()).getId()));
    GameTestHelpers.assertEquals(
        helper,
        message + "helmet",
        npcDataChange.helmet(),
        easyNPC.getMob().getItemBySlot(EquipmentSlot.HEAD).getItem());
    GameTestHelpers.assertTrue(
        helper,
        message + "inventory item",
        easyNPC
                .getEasyNPCInventoryData()
                .getEasyNPCInventory()
                .countItem(npcDataChange.inventoryItem())
            > 0);
    MerchantOffers offers = easyNPC.getEasyNPCTradingData().getTradingOffers();
    GameTestHelpers.assertTrue(
        helper,
        message + "trading offer",
        offers.size() == 1 && offers.get(0).getResult().is(npcDataChange.tradeResult()));
    GameTestHelpers.assertEquals(
        helper,
        message + "objective",
        npcDataChange.followOwner(),
        easyNPC.getEasyNPCObjectiveData().hasObjective(ObjectiveType.FOLLOW_OWNER));
  }

  private static long setBackupFileTimestamp(
      GameTestHelper helper, File backupFile, long timestamp) {
    GameTestHelpers.assertTrue(
        helper,
        "Unable to set the timestamp of the backup file " + backupFile,
        backupFile.setLastModified(timestamp));
    return backupFile.lastModified();
  }

  private static NPCStorageStatistics getStatistics() {
    return NPCEntityManager.getStorageStatistics();
  }

  private static long getLastUpdate(EasyNPC<?> easyNPC) {
    return easyNPC
        .getEasyNPCStatusData()
        .getStatusDataTimestamp(StatusDataType.NPC_DATA_LAST_UPDATE);
  }

  record NPCDataChange(
      String name,
      int lightLevel,
      String dialogName,
      Item helmet,
      Item tradeResult,
      Item inventoryItem,
      boolean followOwner) {}
}
