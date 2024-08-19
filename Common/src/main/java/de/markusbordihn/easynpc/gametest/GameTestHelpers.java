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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.core.BlockPos;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.GameType;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class GameTestHelpers {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private GameTestHelpers() {}

  public static ServerPlayer mockServerPlayer(GameTestHelper helper, Vec3 position) {
    ServerPlayer serverPlayer = helper.makeMockServerPlayerInLevel();
    GameTestHelpers.assertNotNull(helper, "ServerPlayer is null!", serverPlayer);
    serverPlayer.setPos(helper.absoluteVec(position));
    helper.assertEntityPresent(
        serverPlayer.getType(), new BlockPos((int) position.x, (int) position.y, (int) position.z));
    return serverPlayer;
  }

  public static EasyNPC<?> mockEasyNPC(
      GameTestHelper helper, EntityType<? extends PathfinderMob> entityType, Vec3 position) {
    EasyNPC<?> easyNPC = GameTestHelpers.spawnNPCEntityType(helper, entityType);
    GameTestHelpers.assertNotNull(helper, "EasyNPC is null!", easyNPC);
    easyNPC.getEntity().setPos(helper.absoluteVec(position));
    helper.assertEntityPresent(
        easyNPC.getEntity().getType(),
        new BlockPos((int) position.x, (int) position.y, (int) position.z));
    return easyNPC;
  }

  public static EasyNPC<?> spawnNPCEntityType(
      GameTestHelper helper, EntityType<? extends PathfinderMob> entityType) {
    Entity entity = spawnEntityType(helper, entityType);
    if (entity instanceof EasyNPC<?> easyNPC) {
      return easyNPC;
    }
    helper.fail("Entity " + entityType + " is not an EasyNPC!");
    return null;
  }

  public static <T extends Entity> T spawnEntityType(
      GameTestHelper helper, EntityType<? extends PathfinderMob> entityType) {
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
    if (player.level().addFreshEntity(entity)) {
      // helper.assertEntityPresent(entity.getType());
      return entity;
    }

    helper.fail("Failed to spawn entity " + entityType + "!");
    return null;
  }

  public static void assertTrue(GameTestHelper helper, String message, boolean condition) {
    if (condition) {
      helper.succeed();
    } else {
      helper.fail(message);
    }
  }

  public static void assertNotNull(GameTestHelper helper, String message, Object object) {
    assertTrue(helper, message, object != null);
  }
}
