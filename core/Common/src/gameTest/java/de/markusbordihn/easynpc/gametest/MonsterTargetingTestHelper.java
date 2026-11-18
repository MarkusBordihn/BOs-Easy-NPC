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

import de.markusbordihn.easynpc.data.attribute.CombatAttributeType;
import de.markusbordihn.easynpc.entity.ai.goal.NearestAttackableEasyNPCTargetGoal;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.EntityTypes;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.phys.Vec3;

public class MonsterTargetingTestHelper {

  private static final int GROUND_LAYER = 1;

  private static final int TARGET_SEARCH_ATTEMPTS = 200;

  private MonsterTargetingTestHelper() {}

  public static void assertMonsterTargetsAttackableNPC(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockCombatNPC(helper, entityType, true, false);
    Mob monster = spawnMonster(helper);

    GameTestHelpers.assertTrue(
        helper,
        "Monster must be able to attack an NPC that is attackable by monsters",
        monster.canAttack(easyNPC.getLivingEntity()));
    GameTestHelpers.assertTrue(
        helper,
        "Monster must target an NPC that is attackable by monsters",
        searchEasyNPCTarget(monster) == easyNPC.getLivingEntity());
  }

  public static void assertMonsterIgnoresNotAttackableNPC(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockCombatNPC(helper, entityType, false, false);
    Mob monster = spawnMonster(helper);

    GameTestHelpers.assertTrue(
        helper,
        "Monster must not be able to attack an NPC that is not attackable by monsters",
        !monster.canAttack(easyNPC.getLivingEntity()));
    GameTestHelpers.assertTrue(
        helper,
        "Monster must not target an NPC that is not attackable by monsters",
        searchEasyNPCTarget(monster) != easyNPC.getLivingEntity());
  }

  public static void assertMonsterKeepsTargetingInvulnerableNPC(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockCombatNPC(helper, entityType, true, true);
    Mob monster = spawnMonster(helper);

    GameTestHelpers.assertTrue(
        helper,
        "Monster must be able to attack an invulnerable NPC that is attackable by monsters",
        monster.canAttack(easyNPC.getLivingEntity()));

    monster.setTarget(easyNPC.getLivingEntity());
    GameTestHelpers.assertTrue(
        helper,
        "Monster must keep an invulnerable NPC that is attackable by monsters as target",
        new NearestAttackableEasyNPCTargetGoal(monster).canContinueToUse()
            && monster.getTarget() == easyNPC.getLivingEntity());
  }

  private static EasyNPC<?> mockCombatNPC(
      GameTestHelper helper,
      EntityType<?> entityType,
      boolean isAttackableByMonsters,
      boolean isInvulnerable) {
    EasyNPC<?> easyNPC =
        GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, GROUND_LAYER, 1));
    AttributeHandler.setCombatAttribute(
        easyNPC, CombatAttributeType.IS_ATTACKABLE_BY_MONSTERS, isAttackableByMonsters);
    AttributeHandler.setCombatAttribute(
        easyNPC, CombatAttributeType.IS_INVULNERABLE, isInvulnerable);
    return easyNPC;
  }

  private static Mob spawnMonster(GameTestHelper helper) {
    Mob monster = GameTestHelpers.spawnEntityType(helper, EntityTypes.ZOMBIE);
    monster.setNoAi(true);
    monster.setPos(helper.absoluteVec(new Vec3(1, GROUND_LAYER, 1)));
    return monster;
  }

  private static LivingEntity searchEasyNPCTarget(Mob monster) {
    NearestAttackableEasyNPCTargetGoal targetGoal = new NearestAttackableEasyNPCTargetGoal(monster);
    for (int attempt = 0; attempt < TARGET_SEARCH_ATTEMPTS; attempt++) {
      if (targetGoal.canUse()) {
        targetGoal.start();
        break;
      }
    }
    return monster.getTarget();
  }
}
