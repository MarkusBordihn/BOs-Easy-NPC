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
import de.markusbordihn.easynpc.utils.ItemUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.phys.Vec3;

public class MoveEasyNPCItemTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 1, 1);
  private static final BlockPos TARGET_BLOCK_POSITION = new BlockPos(2, 0, 2);

  private MoveEasyNPCItemTestHelper() {}

  public static void assertOwnerMovesNPC(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer owner =
        GameTestHelpers.mockSurvivalServerPlayer(helper, NPC_POSITION, "move-item-owner");
    easyNPC.getEasyNPCOwnerData().setNPCOwnerUUID(owner.getUUID());

    assertMove(helper, easyNPC, owner, InteractionResult.SUCCESS, true);
  }

  public static void assertCreativePlayerMovesNPC(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer creativePlayer =
        GameTestHelpers.mockCreativeServerPlayer(helper, NPC_POSITION, "move-item-creative");

    assertMove(helper, easyNPC, creativePlayer, InteractionResult.SUCCESS, true);
  }

  public static void assertStrangerCannotMoveNPC(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer owner =
        GameTestHelpers.mockSurvivalServerPlayer(helper, NPC_POSITION, "move-item-owner");
    easyNPC.getEasyNPCOwnerData().setNPCOwnerUUID(owner.getUUID());
    ServerPlayer stranger =
        GameTestHelpers.mockSurvivalServerPlayer(helper, NPC_POSITION, "move-item-stranger");

    assertMove(helper, easyNPC, stranger, InteractionResult.PASS, false);
  }

  private static void assertMove(
      GameTestHelper helper,
      EasyNPC<?> easyNPC,
      ServerPlayer serverPlayer,
      InteractionResult expectedResult,
      boolean expectMoved) {
    Item moveItem = ItemUtils.getMoveEasyNPCItem();
    ItemStack moveItemStack = new ItemStack(moveItem);
    Mob mob = easyNPC.getMob();
    Vec3 startPosition = mob.position();

    InteractionResult result =
        moveItem.interactLivingEntity(moveItemStack, serverPlayer, mob, InteractionHand.MAIN_HAND);
    GameTestHelpers.assertEquals(helper, "Move item interaction result", expectedResult, result);

    BlockPos targetBlockPosition = helper.absolutePos(TARGET_BLOCK_POSITION);
    GameTestHelpers.assertTrue(
        helper,
        "Move item must not break the target block",
        !moveItem.canDestroyBlock(
            moveItemStack,
            helper.getLevel().getBlockState(targetBlockPosition),
            helper.getLevel(),
            targetBlockPosition,
            serverPlayer));

    Vec3 expectedPosition = startPosition;
    if (expectMoved) {
      expectedPosition = Vec3.atBottomCenterOf(targetBlockPosition.above());
    }
    GameTestHelpers.assertEquals(
        helper, "NPC position after move", expectedPosition, mob.position());
  }
}
