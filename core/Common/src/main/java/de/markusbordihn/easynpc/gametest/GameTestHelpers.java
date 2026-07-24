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

import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.server.player.FakePlayer;
import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.network.Connection;
import net.minecraft.network.protocol.PacketFlow;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.server.network.CommonListenerCookie;
import net.minecraft.server.network.ServerGamePacketListenerImpl;
import net.minecraft.server.players.PlayerList;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntitySpawnReason;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.GameType;
import net.minecraft.world.phys.Vec3;

public final class GameTestHelpers {

  private GameTestHelpers() {}

  @SuppressWarnings("unchecked")
  public static EasyNPC<?> mockEasyNPC(
      GameTestHelper helper, EntityType<?> entityType, Vec3 position) {
    Mob entity = helper.spawn((EntityType<? extends Mob>) entityType, position);
    if (!(entity instanceof EasyNPC<?> easyNPC)) {
      helper.fail("Spawned entity is not an EasyNPC: " + entity);
      return null;
    }
    return easyNPC;
  }

  public static EasyNPC<?> spawnNPCEntityType(GameTestHelper helper, EntityType<?> entityType) {
    Entity entity = spawnEntityType(helper, entityType);
    if (entity instanceof EasyNPC<?> easyNPC) {
      return easyNPC;
    }

    helper.fail("Entity " + entityType + " is not an EasyNPC!");
    return null;
  }

  @SuppressWarnings("unchecked")
  public static <T extends Entity> T spawnEntityType(
      GameTestHelper helper, EntityType<?> entityType) {
    if (entityType == null) {
      helper.fail("EntityType is null!");
      return null;
    }

    Player player = helper.makeMockPlayer(GameType.DEFAULT_MODE);
    T entity = (T) entityType.create((ServerLevel) player.level(), EntitySpawnReason.COMMAND);
    if (entity == null) {
      helper.fail("Entity for " + entityType + " is null!");
      return null;
    }

    if (!player.level().addFreshEntity(entity)) {
      helper.fail("Failed to spawn entity " + entityType + "!");
      return null;
    }

    return entity;
  }

  public static void assertEquals(
      GameTestHelper helper, String message, Object expected, Object actual) {
    if (!expected.equals(actual)) {
      helper.fail(message + " (expected: " + expected + ", actual: " + actual + ")");
    }
  }

  public static void assertTrue(GameTestHelper helper, String message, boolean condition) {
    if (!condition) {
      helper.fail(message);
    }
  }

  public static void assertNotNull(GameTestHelper helper, String message, Object object) {
    assertTrue(helper, message, object != null);
  }

  public static ServerPlayer mockServerPlayer(GameTestHelper helper, Vec3 position) {
    return createMockServerPlayer(helper, position, "FakePlayer");
  }

  public static ServerPlayer mockServerPlayer(
      GameTestHelper helper, Vec3 position, String playerName) {
    ServerPlayer serverPlayer = createMockServerPlayer(helper, position, playerName);
    // Add the mock player to the level so entity scans, such as faction member lookups, find it.
    helper.getLevel().addNewPlayer(serverPlayer);
    return serverPlayer;
  }

  private static ServerPlayer createMockServerPlayer(
      GameTestHelper helper, Vec3 position, String playerName) {
    ServerLevel level = helper.getLevel();
    // Place the mock player at the structure-relative absolute position so it sits inside its own
    // test area (matching the command source position) and stays far from other tests' mock
    // players,
    // keeping player selectors such as @p deterministic across the shared level.
    BlockPos absolutePosition =
        helper.absolutePos(BlockPos.containing(position.x, position.y, position.z));
    FakePlayer fakePlayer = new FakePlayer(level, absolutePosition, playerName);
    // Attach an unconnected packet listener so command side effects (teleport, feedback) that send
    // client packets do not fail on the mock player, which has no real network connection.
    fakePlayer.connection =
        new ServerGamePacketListenerImpl(
            level.getServer(),
            new Connection(PacketFlow.CLIENTBOUND),
            fakePlayer,
            CommonListenerCookie.createInitial(fakePlayer.getGameProfile(), false));
    registerInPlayerList(level.getServer().getPlayerList(), fakePlayer);
    return fakePlayer;
  }

  /**
   * Registers the mock player in the server player list so that player selectors (e.g. {@code @p})
   * resolve it, without running the full {@code placeNewPlayer} login flow that a real connection
   * would require. This is test-only support and relies on the development runtime mappings.
   */
  @SuppressWarnings("unchecked")
  private static void registerInPlayerList(PlayerList playerList, ServerPlayer serverPlayer) {
    try {
      Field playersField = PlayerList.class.getDeclaredField("players");
      playersField.setAccessible(true);
      Field playersByUuidField = PlayerList.class.getDeclaredField("playersByUUID");
      playersByUuidField.setAccessible(true);
      List<ServerPlayer> players = (List<ServerPlayer>) playersField.get(playerList);
      Map<UUID, ServerPlayer> playersByUuid =
          (Map<UUID, ServerPlayer>) playersByUuidField.get(playerList);
      if (playersByUuid.putIfAbsent(serverPlayer.getUUID(), serverPlayer) == null) {
        players.add(serverPlayer);
      }
    } catch (ReflectiveOperationException exception) {
      throw new IllegalStateException("Unable to register mock player in player list", exception);
    }
  }
}
