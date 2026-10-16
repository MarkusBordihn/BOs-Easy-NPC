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

import de.markusbordihn.easynpc.entity.ModEntityType;
import de.markusbordihn.easynpc.entity.ModNPCEntityType;
import net.fabricmc.fabric.api.gametest.v1.GameTest;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;

@SuppressWarnings("unused")
public class NPCEntityLifecycleTest {

  private static EntityType<?> humanoid() {
    return ModEntityType.getEntityType(ModNPCEntityType.HUMANOID);
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testNPCEntityDataInitialized(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertNPCEntityDataInitialized(helper);
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testNPCIsIndexedOnSpawn(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertNPCIsIndexedOnSpawn(helper, humanoid());
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testRepeatedOwnerUpdateIsIdempotent(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertRepeatedOwnerUpdateIsIdempotent(helper, humanoid());
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testFollowOwnerWithoutTargetIsRegistered(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertFollowOwnerWithoutTargetIsRegistered(helper, humanoid());
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testCustomIdentifierIsQueryable(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertCustomIdentifierIsQueryable(helper, humanoid());
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testRespawnKeepsOwner(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertRespawnKeepsOwner(helper, humanoid());
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testRemovalReasonIsResetOnRespawn(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertRemovalReasonIsResetOnRespawn(helper, humanoid());
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testDeletedNPCIsNotRestorableForOwner(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertDeletedNPCIsNotRestorableForOwner(helper, humanoid());
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testRespawnKeepsIndexEntry(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertRespawnKeepsIndexEntry(helper, humanoid());
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testLoadedNPCStaysPersistent(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertLoadedNPCStaysPersistent(helper, humanoid());
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testNonPersistentNPCIsNotStoredOnJoinAndUnload(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertNonPersistentNPCIsNotStoredOnJoinAndUnload(
        helper, humanoid());
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testNonPersistentNPCIsStoredOnExplicitDespawn(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertNonPersistentNPCIsStoredOnExplicitDespawn(
        helper, humanoid());
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testEnsureInitializedKeepsNPCIndex(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertEnsureInitializedKeepsNPCIndex(helper, humanoid());
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testDeletedNPCIsDiscardedOnLoad(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertDeletedNPCIsDiscardedOnLoad(helper, humanoid());
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testKilledNPCRequiresForcedSpawn(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertKilledNPCRequiresForcedSpawn(helper, humanoid());
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testLoadedNPCIsNotSpawnedTwice(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertLoadedNPCIsNotSpawnedTwice(helper, humanoid());
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testActionGroupsAreRemovedOnLeave(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertActionGroupsAreRemovedOnLeave(helper, humanoid());
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testEasyNPCEntityTypesAreRecognized(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertEasyNPCEntityTypesAreRecognized(helper, humanoid());
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testKilledNPCCanBeRevived(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertKilledNPCCanBeRevived(helper, humanoid());
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testReviveUsesGivenPosition(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertReviveUsesGivenPosition(helper, humanoid());
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3", maxTicks = 100)
  public void testRevivedNPCHasFullHealth(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertRevivedNPCHasFullHealth(helper, humanoid());
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testReviveRejectsDeletedNPC(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertReviveRejectsDeletedNPC(helper, humanoid());
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testReviveRejectsLoadedNPC(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertReviveRejectsLoadedNPC(helper, humanoid());
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void testDefaultDataAppliedOnEveryCreationPath(GameTestHelper helper) {
    NPCEntityLifecycleTestHelper.assertDefaultDataAppliedOnEveryCreationPath(helper, humanoid());
    helper.succeed();
  }
}
