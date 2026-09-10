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
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.data.preset.PresetDataUtils;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.ImportOutcome;
import de.markusbordihn.easynpc.handler.PresetHandler;
import de.markusbordihn.easynpc.handler.PresetImportResult;
import de.markusbordihn.easynpc.security.CommandSecurity;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.UUID;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.phys.Vec3;

public class PresetIdentityTestHelper {

  private static final Vec3 SOURCE_NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 TARGET_NPC_POSITION = new Vec3(2, 2, 2);
  private static final Vec3 SPAWN_POSITION = new Vec3(2, 2, 1);

  private PresetIdentityTestHelper() {}

  public static void assertExportKeepsIdentity(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SOURCE_NPC_POSITION);
    easyNPC.registerEasyNPCDefaultData();

    CompoundTag exportedTag = PresetHandler.prepareExportData(easyNPC);
    GameTestHelpers.assertNotNull(helper, "The NPC must be exportable", exportedTag);
    GameTestHelpers.assertNotNull(
        helper,
        "The export must keep the NPC UUID",
        CompoundTagUtils.readUUID(exportedTag, Entity.TAG_UUID));
    GameTestHelpers.assertEquals(
        helper,
        "The export must keep the NPC UUID",
        easyNPC.getEntityUUID(),
        CompoundTagUtils.readUUID(exportedTag, Entity.TAG_UUID));
    GameTestHelpers.assertTrue(
        helper,
        "The export must keep the NPC position",
        exportedTag.contains(PresetData.POSITION_TAG));
  }

  public static void assertImportRestoresDeletedNPC(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SOURCE_NPC_POSITION);
    easyNPC.registerEasyNPCDefaultData();

    CompoundTag exportedTag = PresetHandler.prepareExportData(easyNPC);
    UUID entityUUID = easyNPC.getEntityUUID();
    Vec3 entityPosition = easyNPC.getEntity().position();
    EasyNPCEntityHandler.delete(easyNPC);
    GameTestHelpers.assertTrue(
        helper,
        "The exported NPC must be gone before the import",
        findNPC(helper, entityUUID) == null);

    GameTestHelpers.assertTrue(
        helper,
        "The exported preset must be importable",
        PresetHandler.importPreset(helper.getLevel(), exportedTag));

    EasyNPC<?> restoredNPC = findNPC(helper, entityUUID);
    GameTestHelpers.assertNotNull(
        helper, "The import must restore the NPC under its original UUID", restoredNPC);
    GameTestHelpers.assertEquals(
        helper,
        "The import must restore the NPC at its original position",
        entityPosition,
        restoredNPC.getEntity().position());
  }

  public static void assertImportNewCreatesNewIdentity(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> sourceNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SOURCE_NPC_POSITION);
    sourceNPC.registerEasyNPCDefaultData();

    CompoundTag exportedTag = PresetHandler.prepareExportData(sourceNPC);
    UUID sourceUUID = sourceNPC.getEntityUUID();
    Vec3 sourcePosition = sourceNPC.getEntity().position();
    UUID newUUID = UUID.randomUUID();
    Vec3 spawnPosition = helper.absoluteVec(SPAWN_POSITION);

    GameTestHelpers.assertTrue(
        helper,
        "The preset must be importable as a new NPC",
        PresetHandler.importPreset(
            helper.getLevel(),
            new PresetData(entityType, exportedTag),
            spawnPosition,
            newUUID,
            CommandSecurity.getServerActorContext(),
            null));

    EasyNPC<?> importedNPC = findNPC(helper, newUUID);
    GameTestHelpers.assertNotNull(
        helper, "The import must create a NPC with the requested UUID", importedNPC);
    GameTestHelpers.assertEquals(
        helper,
        "The new NPC must use the requested position",
        spawnPosition,
        importedNPC.getEntity().position());
    GameTestHelpers.assertEquals(
        helper,
        "The exported NPC must keep its position",
        sourcePosition,
        findNPC(helper, sourceUUID).getEntity().position());
  }

  public static void assertImportKeepsTargetPosition(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> sourceNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SOURCE_NPC_POSITION);
    sourceNPC.registerEasyNPCDefaultData();
    EasyNPC<?> targetNPC = GameTestHelpers.mockEasyNPC(helper, entityType, TARGET_NPC_POSITION);
    targetNPC.registerEasyNPCDefaultData();

    CompoundTag exportedTag = PresetHandler.prepareExportData(sourceNPC);
    UUID targetUUID = targetNPC.getEntityUUID();
    Vec3 targetPosition = targetNPC.getEntity().position();
    long npcCount = LivingEntityManager.getServerEasyNPCEntities().count();

    GameTestHelpers.assertTrue(
        helper,
        "The preset must be importable into an existing NPC",
        PresetHandler.importPreset(
            helper.getLevel(),
            new PresetData(entityType, exportedTag),
            null,
            targetUUID,
            CommandSecurity.getServerActorContext(),
            null));

    EasyNPC<?> updatedNPC = findNPC(helper, targetUUID);
    GameTestHelpers.assertNotNull(helper, "The import must keep the existing NPC", updatedNPC);
    GameTestHelpers.assertEquals(
        helper,
        "The import must not move the existing NPC to the exported position",
        targetPosition,
        updatedNPC.getEntity().position());
    GameTestHelpers.assertEquals(
        helper,
        "The import must not create an additional NPC",
        npcCount,
        LivingEntityManager.getServerEasyNPCEntities().count());
  }

  public static void assertExportExposesIdentity(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SOURCE_NPC_POSITION);
    easyNPC.registerEasyNPCDefaultData();

    PresetData presetData = new PresetData(entityType, PresetHandler.prepareExportData(easyNPC));
    GameTestHelpers.assertEquals(
        helper,
        "The exported preset must expose the NPC UUID",
        easyNPC.getEntityUUID(),
        presetData.getEntityUUID());
    GameTestHelpers.assertEquals(
        helper,
        "The exported preset must expose the NPC position",
        easyNPC.getEntity().position(),
        presetData.getPosition());
  }

  public static void assertImportReportsCreatedEntity(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> sourceNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SOURCE_NPC_POSITION);
    sourceNPC.registerEasyNPCDefaultData();

    PresetData presetData = new PresetData(entityType, PresetHandler.prepareExportData(sourceNPC));
    PresetImportResult importResult =
        importWithReport(helper, presetData, helper.absoluteVec(SPAWN_POSITION), UUID.randomUUID());

    GameTestHelpers.assertEquals(
        helper,
        "The import of a new UUID must report a created NPC",
        ImportOutcome.CREATED,
        importResult.outcome());
    GameTestHelpers.assertTrue(
        helper,
        "The import of a new UUID must not report a preserved identity",
        !importResult.identityPreserved());
  }

  public static void assertImportReportsUpdatedEntity(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SOURCE_NPC_POSITION);
    easyNPC.registerEasyNPCDefaultData();

    PresetData presetData = new PresetData(entityType, PresetHandler.prepareExportData(easyNPC));
    PresetImportResult importResult =
        importWithReport(helper, presetData, null, easyNPC.getEntityUUID());

    GameTestHelpers.assertEquals(
        helper,
        "The import into an existing NPC of the same type must report an update",
        ImportOutcome.UPDATED_EXISTING,
        importResult.outcome());
    GameTestHelpers.assertTrue(
        helper,
        "The import under the stored UUID must report a preserved identity",
        importResult.identityPreserved());
  }

  public static void assertImportReportsReplacedEntity(
      GameTestHelper helper, EntityType<?> entityType, EntityType<?> otherEntityType) {
    EasyNPC<?> sourceNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SOURCE_NPC_POSITION);
    sourceNPC.registerEasyNPCDefaultData();
    EasyNPC<?> targetNPC =
        GameTestHelpers.mockEasyNPC(helper, otherEntityType, TARGET_NPC_POSITION);
    targetNPC.registerEasyNPCDefaultData();

    PresetData presetData = new PresetData(entityType, PresetHandler.prepareExportData(sourceNPC));
    UUID targetUUID = targetNPC.getEntityUUID();
    PresetImportResult importResult = importWithReport(helper, presetData, null, targetUUID);

    GameTestHelpers.assertEquals(
        helper,
        "The import into an existing NPC of another type must report a replacement",
        ImportOutcome.REPLACED_EXISTING,
        importResult.outcome());
    GameTestHelpers.assertEquals(
        helper,
        "The replacement must use the requested UUID",
        targetUUID,
        findNPC(helper, targetUUID).getEntityUUID());
  }

  public static void assertPresetItemDropsIdentity(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SOURCE_NPC_POSITION);
    easyNPC.registerEasyNPCDefaultData();

    CompoundTag exportedTag = PresetHandler.prepareExportData(easyNPC);
    ItemStack itemStack = PresetDataUtils.toItemStack(new PresetData(entityType, exportedTag));
    GameTestHelpers.assertTrue(
        helper, "The preset item must carry the preset", PresetData.has(itemStack));

    CompoundTag itemPreset = PresetData.get(itemStack).data();
    GameTestHelpers.assertTrue(
        helper,
        "The preset item must not carry the NPC UUID",
        !itemPreset.contains(Entity.TAG_UUID));
    GameTestHelpers.assertTrue(
        helper,
        "The preset item must not carry the NPC position",
        !itemPreset.contains(PresetData.POSITION_TAG));
  }

  private static PresetImportResult importWithReport(
      GameTestHelper helper, PresetData presetData, Vec3 position, UUID entityUUID) {
    return PresetHandler.importPresetWithReport(
        helper.getLevel(),
        presetData,
        position,
        entityUUID,
        CommandSecurity.getServerActorContext(),
        null,
        null);
  }

  private static EasyNPC<?> findNPC(GameTestHelper helper, UUID entityUUID) {
    return LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel());
  }
}
