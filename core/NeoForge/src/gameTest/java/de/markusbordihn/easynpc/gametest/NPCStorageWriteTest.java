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

import static de.markusbordihn.easynpc.gametest.GameTestEntityTypes.allay;
import static de.markusbordihn.easynpc.gametest.GameTestEntityTypes.humanoid;
import static de.markusbordihn.easynpc.gametest.GameTestEntityTypes.villager;

import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.Entity;

public final class NPCStorageWriteTest {

  private NPCStorageWriteTest() {}

  public static void testUnchangedReloadWritesNoFile(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertUnchangedReloadWritesNoFile(helper, humanoid());
  }

  public static void testUnchangedReloadWithObjectivesWritesNoFile(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertUnchangedReloadWithObjectivesWritesNoFile(helper, humanoid());
  }

  public static void testUnchangedReloadWithTradingWritesNoFile(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertUnchangedReloadWithTradingWritesNoFile(helper, humanoid());
  }

  public static void testEquipmentChangeIsSaved(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertEquipmentChangeIsSaved(helper, humanoid());
    helper.succeed();
  }

  public static void testCustomNameChangeIsSaved(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertCustomNameChangeIsSaved(helper, humanoid());
    helper.succeed();
  }

  public static void testCustomNameRemovalIsSaved(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertCustomNameRemovalIsSaved(helper, humanoid());
    helper.succeed();
  }

  public static void testInventoryChangeIsSaved(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertInventoryChangeIsSaved(helper, humanoid());
    helper.succeed();
  }

  public static void testItemPickupIsSaved(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertItemPickupIsSaved(helper, humanoid());
    helper.succeed();
  }

  public static void testPresetImportIsSaved(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertPresetImportIsSaved(helper, humanoid());
    helper.succeed();
  }

  public static void testPresetImportWithTypeChangeIsSaved(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertPresetImportWithTypeChangeIsSaved(
        helper, humanoid(), villager());
    helper.succeed();
  }

  public static void testLoadOnRunningNPCIsSaved(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertLoadOnRunningNPCIsSaved(helper, humanoid());
    helper.succeed();
  }

  public static void testKilledNPCIsStillSaved(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertRemovalIsStillSaved(
        helper, humanoid(), Entity.RemovalReason.KILLED, NPCRemovalReason.KILLED);
    helper.succeed();
  }

  public static void testUnloadWithPlayerIsStillSaved(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertRemovalIsStillSaved(
        helper,
        humanoid(),
        Entity.RemovalReason.UNLOADED_WITH_PLAYER,
        NPCRemovalReason.UNLOADED_WITH_PLAYER);
    helper.succeed();
  }

  public static void testDimensionChangeIsStillSaved(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertRemovalIsStillSaved(
        helper,
        humanoid(),
        Entity.RemovalReason.CHANGED_DIMENSION,
        NPCRemovalReason.CHANGED_DIMENSION);
    helper.succeed();
  }

  public static void testBackupRunSkipsUnchangedNPC(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertBackupRunSkipsUnchangedNPC(helper, humanoid());
    helper.succeed();
  }

  public static void testBackupRunExportsChangedNPC(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertBackupRunExportsChangedNPC(helper, humanoid());
  }

  public static void testListCommandReadsNoNPCFile(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertListCommandReadsNoNPCFile(helper, humanoid());
    helper.succeed();
  }

  public static void testRepeatedSaveKeepsIndexClean(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertRepeatedSaveKeepsIndexClean(helper, humanoid());
    helper.succeed();
  }

  public static void testUnchangedPauseStateKeepsNPCClean(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertUnchangedPauseStateKeepsNPCClean(helper, humanoid());
    helper.succeed();
  }

  public static void testShoulderRoundTripWritesNoUnchangedFile(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertShoulderRoundTripWritesNoUnchangedFile(helper, allay());
    helper.succeed();
  }

  public static void testShoulderRoundTripsKeepLatestChanges(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertShoulderRoundTripsKeepLatestChanges(helper, allay());
    helper.succeed();
  }

  public static void testDialogChangeIsSaved(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertDialogChangeIsSaved(helper, humanoid());
    helper.succeed();
  }

  public static void testDialogRemovalIsSaved(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertDialogRemovalIsSaved(helper, humanoid());
    helper.succeed();
  }

  public static void testObjectiveChangeIsSaved(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertObjectiveChangeIsSaved(helper, humanoid());
    helper.succeed();
  }

  public static void testObjectiveRemovalIsSaved(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertObjectiveRemovalIsSaved(helper, humanoid());
    helper.succeed();
  }

  public static void testObjectiveRefreshKeepsNPCClean(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertObjectiveRefreshKeepsNPCClean(helper, humanoid());
    helper.succeed();
  }

  public static void testHourlyFlushKeepsUnchangedNPCClean(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertHourlyFlushKeepsUnchangedNPCClean(helper, humanoid());
  }

  public static void testHourlyFlushSavesUnmarkedChange(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertHourlyFlushSavesUnmarkedChange(helper, humanoid());
  }

  public static void testBackupRunExportsUnmarkedChange(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertBackupRunExportsUnmarkedChange(helper, humanoid());
  }

  public static void testFileStorageStatsCommandRuns(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertFileStorageStatsCommandRuns(helper);
    helper.succeed();
  }

  public static void testMassReloadWritesNoUnchangedNPCFile(GameTestHelper helper) {
    NPCStorageWriteTestHelper.assertMassReloadWritesNoUnchangedNPCFile(helper, humanoid());
  }
}
