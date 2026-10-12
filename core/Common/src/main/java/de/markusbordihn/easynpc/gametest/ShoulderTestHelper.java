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

import de.markusbordihn.easynpc.api.event.EasyNPCEventRegistry;
import de.markusbordihn.easynpc.api.event.ShoulderEventListener;
import de.markusbordihn.easynpc.api.handler.EasyNPCAttributeHandler;
import de.markusbordihn.easynpc.api.handler.EasyNPCEntityHandler;
import de.markusbordihn.easynpc.api.handler.EasyNPCObjectiveHandler;
import de.markusbordihn.easynpc.api.handler.EasyNPCShoulderHandler;
import de.markusbordihn.easynpc.data.attribute.InteractionAttributeType;
import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import de.markusbordihn.easynpc.data.npc.ShoulderSide;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.NPCEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.SitOnOwnerShoulderGoal;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.phys.Vec3;

public class ShoulderTestHelper {

  private ShoulderTestHelper() {}

  private static ServerPlayer mockGroundedPlayer(GameTestHelper helper, String playerName) {
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, new Vec3(2, 2, 1), playerName);
    serverPlayer.setOnGround(true);
    return serverPlayer;
  }

  private static EasyNPC<?> mockStoredNPC(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    NPCEntityManager.saveNPC(easyNPC);
    return easyNPC;
  }

  private static Optional<NPCRemovalReason> getStoredRemovalReason(UUID entityUUID) {
    return NPCEntityManager.getNPC(entityUUID).map(entry -> entry.metadata().removalReason());
  }

  public static void assertSitOnShoulderStoresNPC(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);
    ServerPlayer serverPlayer = mockGroundedPlayer(helper, "shoulder-sit-player");
    UUID entityUUID = easyNPC.getEntityUUID();

    if (!EasyNPCShoulderHandler.sitOnShoulder(easyNPC, serverPlayer)) {
      helper.fail("NPC " + entityUUID + " did not sit on the shoulder of a grounded player");
      return;
    }

    if (!easyNPC.getEntity().isRemoved()) {
      helper.fail("NPC is still in the world while it sits on the shoulder");
      return;
    }

    Optional<UUID> shoulderUUID =
        EasyNPCShoulderHandler.getShoulderNPCUUID(serverPlayer, ShoulderSide.LEFT);
    if (shoulderUUID.isEmpty() || !shoulderUUID.get().equals(entityUUID)) {
      helper.fail("Left shoulder holds " + shoulderUUID + " instead of NPC " + entityUUID);
      return;
    }

    Optional<NPCRemovalReason> removalReason = getStoredRemovalReason(entityUUID);
    if (removalReason.isEmpty() || removalReason.get() != NPCRemovalReason.ON_SHOULDER) {
      helper.fail("NPC on the shoulder is stored with the removal reason " + removalReason);
      return;
    }

    if (!EasyNPCShoulderHandler.isOnShoulder(entityUUID)) {
      helper.fail("NPC " + entityUUID + " is not reported as sitting on a shoulder");
      return;
    }

    if (EasyNPCEntityHandler.spawn(entityUUID, helper.getLevel())) {
      helper.fail("NPC on the shoulder was spawned a second time");
    }
  }

  public static void assertSecondNPCUsesRightShoulder(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> leftNPC = mockStoredNPC(helper, entityType);
    EasyNPC<?> rightNPC = mockStoredNPC(helper, entityType);
    EasyNPC<?> thirdNPC = mockStoredNPC(helper, entityType);
    ServerPlayer serverPlayer = mockGroundedPlayer(helper, "shoulder-both-player");

    if (!EasyNPCShoulderHandler.sitOnShoulder(leftNPC, serverPlayer)
        || !EasyNPCShoulderHandler.sitOnShoulder(rightNPC, serverPlayer)) {
      helper.fail("Two NPCs did not sit on both shoulders");
      return;
    }

    Optional<UUID> rightUUID =
        EasyNPCShoulderHandler.getShoulderNPCUUID(serverPlayer, ShoulderSide.RIGHT);
    if (rightUUID.isEmpty() || !rightUUID.get().equals(rightNPC.getEntityUUID())) {
      helper.fail("Right shoulder holds " + rightUUID + " instead of the second NPC");
      return;
    }

    if (EasyNPCShoulderHandler.sitOnShoulder(thirdNPC, serverPlayer)
        || thirdNPC.getEntity().isRemoved()) {
      helper.fail("A third NPC sat on a player whose shoulders are both taken");
    }
  }

  public static void assertDismountRespawnsNPC(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);
    ServerPlayer serverPlayer = mockGroundedPlayer(helper, "shoulder-dismount-player");
    UUID entityUUID = easyNPC.getEntityUUID();
    AtomicReference<UUID> dismountedUUID = new AtomicReference<>();
    ShoulderEventListener shoulderEventListener =
        new ShoulderEventListener() {
          @Override
          public void onShoulderMounted(
              EasyNPC<?> mountedNPC, Player player, ShoulderSide shoulderSide) {}

          @Override
          public void onShoulderDismounted(EasyNPC<?> dismountedNPC) {
            dismountedUUID.set(dismountedNPC.getEntityUUID());
          }
        };
    EasyNPCEventRegistry.registerShoulderEventListener(shoulderEventListener);
    GameTestHelpers.addCleanup(
        helper, () -> EasyNPCEventRegistry.unregisterShoulderEventListener(shoulderEventListener));

    if (!EasyNPCShoulderHandler.sitOnShoulder(easyNPC, serverPlayer)) {
      helper.fail("NPC " + entityUUID + " did not sit on the shoulder");
      return;
    }

    if (!EasyNPCShoulderHandler.dismount(serverPlayer, ShoulderSide.LEFT)) {
      helper.fail("NPC " + entityUUID + " was not dismounted from the left shoulder");
      return;
    }

    if (!serverPlayer.getShoulderEntityLeft().isEmpty()) {
      helper.fail("Left shoulder still holds the NPC after the dismount");
      return;
    }

    if (LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel()) == null) {
      helper.fail("Dismounted NPC " + entityUUID + " was not found in the world");
      return;
    }

    Optional<NPCRemovalReason> removalReason = getStoredRemovalReason(entityUUID);
    if (removalReason.isEmpty() || removalReason.get() != NPCRemovalReason.NONE) {
      helper.fail("Dismounted NPC still uses the removal reason " + removalReason);
      return;
    }

    if (!entityUUID.equals(dismountedUUID.get())) {
      helper.fail("Dismount event was not fired for NPC " + entityUUID);
    }
  }

  public static void assertShoulderSizeLimit(
      GameTestHelper helper, EntityType<?> smallEntityType, EntityType<?> largeEntityType) {
    EasyNPC<?> smallNPC = mockStoredNPC(helper, smallEntityType);
    EasyNPC<?> largeNPC = mockStoredNPC(helper, largeEntityType);

    if (!SitOnOwnerShoulderGoal.fitsOnShoulder(smallNPC.getEntity())) {
      helper.fail("Small NPC " + smallEntityType + " does not fit on a shoulder");
      return;
    }

    if (SitOnOwnerShoulderGoal.fitsOnShoulder(largeNPC.getEntity())) {
      helper.fail("Large NPC " + largeEntityType + " fits on a shoulder");
    }
  }

  public static void assertAttributeTogglesObjective(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);

    EasyNPCAttributeHandler.setInteractionAttribute(
        easyNPC, InteractionAttributeType.CAN_SIT_ON_SHOULDER, true);
    if (!EasyNPCObjectiveHandler.hasObjective(easyNPC, ObjectiveType.SIT_ON_OWNER_SHOULDER)) {
      helper.fail("Enabling \"Sit on Shoulder\" did not add the shoulder objective");
      return;
    }

    EasyNPCAttributeHandler.setInteractionAttribute(
        easyNPC, InteractionAttributeType.CAN_SIT_ON_SHOULDER, false);
    if (EasyNPCObjectiveHandler.hasObjective(easyNPC, ObjectiveType.SIT_ON_OWNER_SHOULDER)) {
      helper.fail("Disabling \"Sit on Shoulder\" did not remove the shoulder objective");
    }
  }
}
