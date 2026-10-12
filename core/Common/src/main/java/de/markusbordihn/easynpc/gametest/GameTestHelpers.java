/*
 * Copyright 2023 Markus Bordihn
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

import com.mojang.authlib.GameProfile;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.saveddata.FactionData;
import de.markusbordihn.easynpc.entity.NPCEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.FactionHandler;
import io.netty.channel.embedded.EmbeddedChannel;
import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.gametest.framework.GameTestInfo;
import net.minecraft.gametest.framework.GameTestListener;
import net.minecraft.gametest.framework.GameTestRunner;
import net.minecraft.network.Connection;
import net.minecraft.network.protocol.PacketFlow;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.server.network.CommonListenerCookie;
import net.minecraft.server.players.PlayerList;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.GameType;
import net.minecraft.world.phys.Vec3;
import net.minecraft.world.scores.PlayerTeam;
import net.minecraft.world.scores.Scoreboard;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class GameTestHelpers {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  // Vanilla 1.20.1 GameTestHelper has no addCleanup; its field name differs per mapping.
  private static final Field TEST_INFO_FIELD = findTestInfoField();

  private GameTestHelpers() {}

  private static Field findTestInfoField() {
    for (Field field : GameTestHelper.class.getDeclaredFields()) {
      if (field.getType() == GameTestInfo.class) {
        field.setAccessible(true);
        return field;
      }
    }
    throw new IllegalStateException("GameTestHelper has no GameTestInfo field");
  }

  public static void addCleanup(GameTestHelper helper, Runnable cleanup) {
    GameTestInfo testInfo;
    try {
      testInfo = (GameTestInfo) TEST_INFO_FIELD.get(helper);
    } catch (IllegalAccessException e) {
      throw new IllegalStateException("Unable to access the GameTestInfo of " + helper, e);
    }

    testInfo.addListener(
        new GameTestListener() {
          @Override
          public void testStructureLoaded(GameTestInfo loadedTestInfo) {}

          @Override
          public void testPassed(GameTestInfo passedTestInfo, GameTestRunner gameTestRunner) {
            runCleanup(passedTestInfo, cleanup);
          }

          @Override
          public void testFailed(GameTestInfo failedTestInfo, GameTestRunner gameTestRunner) {
            runCleanup(failedTestInfo, cleanup);
          }

          @Override
          public void testAddedForRerun(
              GameTestInfo previousTestInfo,
              GameTestInfo rerunTestInfo,
              GameTestRunner gameTestRunner) {}
        });
  }

  private static void runCleanup(GameTestInfo testInfo, Runnable cleanup) {
    try {
      cleanup.run();
    } catch (RuntimeException e) {
      log.error("Cleanup of game test {} failed:", testInfo.getTestName(), e);
    }
  }

  public static void removeFactionsAfterTest(GameTestHelper helper, String... factionNames) {
    Scoreboard scoreboard = helper.getLevel().getScoreboard();
    List<String> createdFactionNames =
        Arrays.stream(factionNames)
            .filter(factionName -> !FactionData.get().hasFaction(factionName))
            .toList();
    List<String> createdTeamNames =
        Arrays.stream(factionNames)
            .filter(factionName -> scoreboard.getPlayerTeam(factionName) == null)
            .toList();

    addCleanup(
        helper,
        () -> {
          createdFactionNames.forEach(FactionHandler::deleteFaction);
          for (String teamName : createdTeamNames) {
            PlayerTeam playerTeam = scoreboard.getPlayerTeam(teamName);
            if (playerTeam != null) {
              scoreboard.removePlayerTeam(playerTeam);
            }
          }
        });
  }

  public static ServerPlayer mockServerPlayer(GameTestHelper helper, Vec3 position) {
    return mockServerPlayer(helper, position, "test-mock-player");
  }

  public static ServerPlayer mockServerPlayer(
      GameTestHelper helper, Vec3 position, String playerName) {
    return mockServerPlayer(helper, position, playerName, true);
  }

  public static ServerPlayer mockSurvivalServerPlayer(
      GameTestHelper helper, Vec3 position, String playerName) {
    return mockServerPlayer(helper, position, playerName, false);
  }

  private static ServerPlayer mockServerPlayer(
      GameTestHelper helper, Vec3 position, String playerName, boolean creative) {
    ServerPlayer serverPlayer = makeConnectedMockServerPlayer(helper, playerName, creative);
    GameTestHelpers.assertNotNull(helper, "ServerPlayer is null!", serverPlayer);
    serverPlayer.setPos(helper.absoluteVec(position));
    helper.assertEntityPresent(
        serverPlayer.getType(), new BlockPos((int) position.x, (int) position.y, (int) position.z));
    return serverPlayer;
  }

  private static ServerPlayer makeConnectedMockServerPlayer(
      GameTestHelper helper, String playerName, boolean creative) {
    ServerLevel level = helper.getLevel();
    CommonListenerCookie listenerCookie =
        CommonListenerCookie.createInitial(new GameProfile(UUID.randomUUID(), playerName), false);
    ServerPlayer serverPlayer =
        new ServerPlayer(
            level.getServer(),
            level,
            listenerCookie.gameProfile(),
            listenerCookie.clientInformation()) {
          @Override
          public boolean isSpectator() {
            return false;
          }

          @Override
          public boolean isCreative() {
            return creative;
          }
        };
    Connection connection = new Connection(PacketFlow.SERVERBOUND);
    new EmbeddedChannel(connection);
    PlayerList playerList = level.getServer().getPlayerList();
    playerList.placeNewPlayer(connection, serverPlayer, listenerCookie);
    addCleanup(
        helper,
        () -> {
          level.getScoreboard().removePlayerFromTeam(serverPlayer.getScoreboardName());
          playerList.remove(serverPlayer);
        });
    return serverPlayer;
  }

  public static EasyNPC<?> mockEasyNPC(
      GameTestHelper helper, EntityType<?> entityType, Vec3 position) {
    EasyNPC<?> easyNPC = GameTestHelpers.spawnNPCEntityType(helper, entityType);
    GameTestHelpers.assertNotNull(helper, "EasyNPC is null!", easyNPC);
    easyNPC.getEntity().setPos(helper.absoluteVec(position));
    helper.assertEntityPresent(
        easyNPC.getEntity().getType(),
        new BlockPos((int) position.x, (int) position.y, (int) position.z));
    return easyNPC;
  }

  public static void removeNPCDataAfterTest(GameTestHelper helper, UUID entityUUID) {
    addCleanup(helper, () -> NPCEntityManager.removeNPC(entityUUID));
  }

  public static EasyNPC<?> spawnNPCEntityType(GameTestHelper helper, EntityType<?> entityType) {
    Entity entity = spawnEntityType(helper, entityType);
    if (entity instanceof EasyNPC<?> easyNPC) {
      removeNPCDataAfterTest(helper, easyNPC.getEntityUUID());
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
    T entity = (T) entityType.create(player.level());
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
}
