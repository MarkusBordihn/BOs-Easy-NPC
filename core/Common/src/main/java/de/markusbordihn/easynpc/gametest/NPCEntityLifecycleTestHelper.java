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
import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import de.markusbordihn.easynpc.data.npc.SavedNPCEntityEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.data.saveddata.NPCEntityData;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.NPCEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.handler.OwnerHandler;
import java.util.Collection;
import java.util.UUID;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class NPCEntityLifecycleTestHelper {

  private NPCEntityLifecycleTestHelper() {}

  private static boolean containsNPC(Collection<SavedNPCEntityEntry> entries, UUID entityUUID) {
    return entries.stream().anyMatch(entry -> entityUUID.equals(entry.entityUUID()));
  }

  // A loader without the server started event makes every NPC query silently return nothing.
  public static void assertNPCEntityDataInitialized(GameTestHelper helper) {
    try {
      NPCEntityData.get();
    } catch (IllegalStateException e) {
      helper.fail("NPCEntityData is not initialized after server start: " + e.getMessage());
      return;
    }

    if (NPCEntityManager.getAllNPCs() == null) {
      helper.fail("NPCEntityManager is not usable after server start");
    }
  }

  public static void assertNPCIsIndexedOnSpawn(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    NPCEntityManager.saveNPC(easyNPC);

    if (NPCEntityManager.getNPC(easyNPC.getEntityUUID()).isEmpty()) {
      helper.fail("Spawned NPC " + easyNPC.getEntityUUID() + " is missing from the NPC index");
    }
  }

  public static void assertOwnerIndexUpdate(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, new Vec3(2, 2, 1), "owner-index-player");
    NPCEntityManager.saveNPC(easyNPC);

    UUID entityUUID = easyNPC.getEntityUUID();
    UUID ownerUUID = serverPlayer.getUUID();
    if (!OwnerHandler.setOwner(easyNPC, serverPlayer)) {
      helper.fail("Failed to set the owner of the NPC");
      return;
    }

    if (!containsNPC(EasyNPCEntityHandler.getByOwner(ownerUUID), entityUUID)) {
      helper.fail("NPC is missing from the owner index after setting the owner");
    }

    if (!OwnerHandler.removeOwner(easyNPC)) {
      helper.fail("Failed to remove the owner of the NPC");
      return;
    }

    if (containsNPC(EasyNPCEntityHandler.getByOwner(ownerUUID), entityUUID)) {
      helper.fail("NPC is still in the owner index after removing the owner");
    }
  }

  public static void assertRepeatedOwnerUpdateIsIdempotent(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, new Vec3(2, 2, 1), "owner-repeat-player");
    NPCEntityManager.saveNPC(easyNPC);

    OwnerHandler.setOwner(easyNPC, serverPlayer);
    if (!OwnerHandler.setOwner(easyNPC, serverPlayer)) {
      helper.fail("Setting the same owner again should succeed");
      return;
    }

    OwnerDataCapable<?> ownerData = easyNPC.getEasyNPCOwnerData();
    if (ownerData == null || !serverPlayer.getUUID().equals(ownerData.getOwnerUUID())) {
      helper.fail("Owner must stay unchanged after setting the same owner again");
    }
  }

  public static void assertFollowOwnerWithoutTargetIsRegistered(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, new Vec3(2, 2, 1), "follow-owner-player");
    ObjectiveDataCapable<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    if (objectiveData == null) {
      helper.fail("NPC has no objective data");
      return;
    }

    ObjectiveDataEntry followOwner = new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER);
    objectiveData.addOrUpdateCustomObjective(followOwner);
    if (followOwner.isRegistered()) {
      helper.fail("Follow owner objective must stay unregistered while the NPC has no owner");
    }

    OwnerHandler.setOwner(easyNPC, serverPlayer);

    ObjectiveDataEntry registeredObjective = objectiveData.getObjective(ObjectiveType.FOLLOW_OWNER);
    if (registeredObjective == null || !registeredObjective.isRegistered()) {
      helper.fail("Follow owner objective was not registered after the owner was set");
      return;
    }

    if (registeredObjective.getTargetOwnerUUID() != null) {
      helper.fail("Follow owner objective must stay owner agnostic, got a fixed target owner");
    }
  }

  public static void assertCustomIdentifierIsQueryable(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    Identifier customIdentifier = Identifier.fromNamespaceAndPath("othermod", "companion");
    if (easyNPC.getEasyNPCPresetData() == null) {
      helper.fail("NPC has no preset data");
      return;
    }

    if (!EasyNPC.DEFAULT_CUSTOM_NPC_IDENTIFIER.equals(easyNPC.getCustomNPCIdentifier())) {
      helper.fail("NPC without a custom identifier should report the default one");
    }

    easyNPC.getEasyNPCPresetData().setCustomIdentifier(customIdentifier);
    NPCEntityManager.saveNPC(easyNPC);

    if (!customIdentifier.equals(easyNPC.getCustomNPCIdentifier())) {
      helper.fail(
          "NPC did not report its custom identifier, got " + easyNPC.getCustomNPCIdentifier());
    }
    if (!containsNPC(
        EasyNPCEntityHandler.getByCustomIdentifier(customIdentifier), easyNPC.getEntityUUID())) {
      helper.fail("NPC is missing from the custom identifier index");
    }
    if (!containsNPC(
        EasyNPCEntityHandler.getByCustomIdentifierNamespace("othermod"), easyNPC.getEntityUUID())) {
      helper.fail("NPC is missing from the custom identifier namespace index");
    }
  }

  public static void assertRespawnKeepsOwner(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, new Vec3(2, 2, 1), "respawn-owner-player");
    NPCEntityManager.saveNPC(easyNPC);
    OwnerHandler.setOwner(easyNPC, serverPlayer);
    NPCEntityManager.saveNPC(easyNPC);

    UUID entityUUID = easyNPC.getEntityUUID();
    if (!EasyNPCEntityHandler.despawn(easyNPC, NPCRemovalReason.DESPAWNED)) {
      helper.fail("Failed to despawn the NPC");
      return;
    }

    if (!EasyNPCEntityHandler.spawn(entityUUID, helper.getLevel())) {
      helper.fail("Failed to respawn the NPC");
      return;
    }

    EasyNPC<?> respawnedNPC =
        LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel());
    if (respawnedNPC == null) {
      helper.fail("Respawned NPC " + entityUUID + " was not found in the world");
      return;
    }

    OwnerDataCapable<?> ownerData = respawnedNPC.getEasyNPCOwnerData();
    if (ownerData == null || !serverPlayer.getUUID().equals(ownerData.getOwnerUUID())) {
      helper.fail(
          "Respawned NPC lost its owner, expected "
              + serverPlayer.getUUID()
              + ", got "
              + (ownerData != null ? ownerData.getOwnerUUID() : null));
    }
  }
}
