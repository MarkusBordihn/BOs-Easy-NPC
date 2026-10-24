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
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.LivingEntityEvents;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.handler.OwnerHandler;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class NPCMassLifecycleTestHelper {

  public static final int NPC_COUNT = 200;

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[Mass NPC Lifecycle Test]";

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 PLAYER_POSITION = new Vec3(2, 2, 1);

  private static final long MAX_SPAWN_DURATION_MILLIS = 20000;
  private static final long MAX_PLAYER_LEAVE_DURATION_MILLIS = 5000;
  private static final long MAX_REMOVAL_DURATION_MILLIS = 20000;

  private NPCMassLifecycleTestHelper() {}

  private static List<EasyNPC<?>> spawnNPCs(GameTestHelper helper, EntityType<?> entityType) {
    List<EasyNPC<?>> spawnedNPCs = new ArrayList<>(NPC_COUNT);
    for (int i = 0; i < NPC_COUNT; i++) {
      EasyNPC<?> easyNPC = GameTestHelpers.spawnNPCEntityType(helper, entityType);
      easyNPC.getEntity().setPos(helper.absoluteVec(NPC_POSITION));
      spawnedNPCs.add(easyNPC);
    }
    return spawnedNPCs;
  }

  private static void makeOwnerEventListener(EasyNPC<?> easyNPC, ServerPlayer serverPlayer) {
    ObjectiveDataCapable<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    objectiveData.addOrUpdateCustomObjective(new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER));
    OwnerHandler.setOwner(easyNPC, serverPlayer);
    objectiveData.refreshCustomObjectives();
  }

  private static void makeEntityEventListener(EasyNPC<?> easyNPC, UUID targetEntityUUID) {
    ObjectiveDataEntry objectiveDataEntry =
        new ObjectiveDataEntry(ObjectiveType.LOOK_AT_ENTITY_BY_UUID);
    objectiveDataEntry.setTargetEntityUUID(targetEntityUUID);
    ObjectiveDataCapable<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    objectiveData.addOrUpdateCustomObjective(objectiveDataEntry);
    objectiveData.refreshCustomObjectives();
  }

  private static void assertWithinBudget(
      GameTestHelper helper, String step, long durationMillis, long budgetMillis) {
    log.info("{} {} of {} NPCs took {} ms", LOG_PREFIX, step, NPC_COUNT, durationMillis);
    if (durationMillis > budgetMillis) {
      helper.fail(
          step
              + " of "
              + NPC_COUNT
              + " NPCs took "
              + durationMillis
              + " ms, which is above the budget of "
              + budgetMillis
              + " ms");
    }
  }

  public static void assertMassSpawnStaysResponsive(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, "mass-spawn-player");

    long startTime = System.currentTimeMillis();
    for (EasyNPC<?> easyNPC : spawnNPCs(helper, entityType)) {
      makeOwnerEventListener(easyNPC, serverPlayer);
    }
    assertWithinBudget(
        helper, "Spawn", System.currentTimeMillis() - startTime, MAX_SPAWN_DURATION_MILLIS);

    long trackedNPCs = LivingEntityManager.getServerEasyNPCEntities().count();
    if (trackedNPCs < NPC_COUNT) {
      helper.fail("Only " + trackedNPCs + " of " + NPC_COUNT + " NPCs are tracked after the spawn");
    }
  }

  public static void assertMassPlayerLeaveStaysResponsive(
      GameTestHelper helper, EntityType<?> entityType) {
    String playerName = "mass-leave-player";
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, playerName);
    for (EasyNPC<?> easyNPC : spawnNPCs(helper, entityType)) {
      makeOwnerEventListener(easyNPC, serverPlayer);
    }

    long startTime = System.currentTimeMillis();
    LivingEntityEvents.handleLivingEntityLeaveEvent(serverPlayer);
    assertWithinBudget(
        helper,
        "Player leave",
        System.currentTimeMillis() - startTime,
        MAX_PLAYER_LEAVE_DURATION_MILLIS);

    if (LivingEntityManager.getPlayerByName(playerName) != null) {
      helper.fail("Player is still tracked after the leave event");
    }
  }

  public static void assertMassRemovalStaysResponsive(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, "mass-removal-player");
    List<EasyNPC<?>> spawnedNPCs = spawnNPCs(helper, entityType);
    for (EasyNPC<?> easyNPC : spawnedNPCs) {
      makeEntityEventListener(easyNPC, serverPlayer.getUUID());
    }

    long startTime = System.currentTimeMillis();
    for (EasyNPC<?> easyNPC : spawnedNPCs) {
      easyNPC.getEntity().setRemoved(Entity.RemovalReason.UNLOADED_TO_CHUNK);
    }
    assertWithinBudget(
        helper, "Removal", System.currentTimeMillis() - startTime, MAX_REMOVAL_DURATION_MILLIS);

    for (EasyNPC<?> easyNPC : spawnedNPCs) {
      if (LivingEntityManager.getServerEasyNPCEntityByUUID(easyNPC.getEntityUUID()) != null) {
        helper.fail("NPC " + easyNPC.getEntityUUID() + " is still tracked after its removal");
        return;
      }
    }
  }
}
