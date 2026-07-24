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

import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.data.objective.ObjectiveUtils;
import de.markusbordihn.easynpc.data.saveddata.FactionData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.CustomFactionHurtByTargetGoal;
import de.markusbordihn.easynpc.handler.FactionHandler;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.phys.Vec3;
import net.minecraft.world.scores.PlayerTeam;
import net.minecraft.world.scores.Scoreboard;

public class FactionDefenseTestHelper {

  private static final int IMMEDIATE_SCAN_INTERVAL = 1;
  private static final int TICKS_BEFORE_ATTACK = 2;

  private FactionDefenseTestHelper() {}

  public static void assertFactionDefenseTargetsOutsideAttacker(
      GameTestHelper helper, EntityType<?> entityType) {
    String factionName = "guards-outside-attacker";
    FactionData.init(helper.getLevel().getServer());
    EasyNPC<?> defender = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 1));
    EasyNPC<?> factionMember = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));
    FactionHandler.setFaction(defender, factionName);
    FactionHandler.setFaction(factionMember, factionName);

    LivingEntity outsider = spawnOutsideAttacker(helper, new Vec3(1, 2, 1));
    factionMember.getLivingEntity().setLastHurtByMob(outsider);

    CustomFactionHurtByTargetGoal<?> goal =
        new CustomFactionHurtByTargetGoal<>(defender, IMMEDIATE_SCAN_INTERVAL);
    GameTestHelpers.assertTrue(
        helper, "Defender must react to an attack on a faction member", goal.canUse());

    goal.start();
    GameTestHelpers.assertTrue(
        helper,
        "Defender must target the attacker of the faction member",
        defender.getMob().getTarget() == outsider);
  }

  public static void assertFactionDefenseIgnoresInternalDispute(
      GameTestHelper helper, EntityType<?> entityType) {
    String factionName = "guards-internal-dispute";
    FactionData.init(helper.getLevel().getServer());
    EasyNPC<?> defender = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 1));
    EasyNPC<?> factionMember = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));
    FactionHandler.setFaction(defender, factionName);
    FactionHandler.setFaction(factionMember, factionName);

    ServerPlayer factionPlayer =
        GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1), "dispute-player");
    joinFaction(helper, factionPlayer, factionName);
    factionMember.getLivingEntity().setLastHurtByMob(factionPlayer);

    GameTestHelpers.assertEquals(
        helper,
        "Attacking player is not in the faction",
        factionName,
        FactionHandler.getTargetGroupName(factionPlayer));

    CustomFactionHurtByTargetGoal<?> goal =
        new CustomFactionHurtByTargetGoal<>(defender, IMMEDIATE_SCAN_INTERVAL);
    GameTestHelpers.assertTrue(
        helper, "Faction must stay out of a fight between two of its own members", !goal.canUse());
    GameTestHelpers.assertTrue(
        helper,
        "Defender must not target a member of its own faction",
        defender.getMob().getTarget() == null);
  }

  public static void assertFactionDefenseDefendsFactionPlayer(
      GameTestHelper helper, EntityType<?> entityType) {
    String factionName = "guards-player-defense";
    FactionData.init(helper.getLevel().getServer());
    EasyNPC<?> defender = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 1));
    FactionHandler.setFaction(defender, factionName);

    ServerPlayer factionPlayer =
        GameTestHelpers.mockServerPlayer(helper, new Vec3(2, 2, 2), "defended-player");
    joinFaction(helper, factionPlayer, factionName);
    LivingEntity outsider = spawnOutsideAttacker(helper, new Vec3(1, 2, 1));
    factionPlayer.setLastHurtByMob(outsider);

    CustomFactionHurtByTargetGoal<?> goal =
        new CustomFactionHurtByTargetGoal<>(defender, IMMEDIATE_SCAN_INTERVAL);
    GameTestHelpers.assertTrue(
        helper, "Defender must react to an attack on a player of the own faction", goal.canUse());

    goal.start();
    GameTestHelpers.assertTrue(
        helper,
        "Defender must target the attacker of the faction player",
        defender.getMob().getTarget() == outsider);
  }

  public static void assertFactionDefenseRequiresFaction(
      GameTestHelper helper, EntityType<?> entityType) {
    String factionName = "guards-requires-faction";
    FactionData.init(helper.getLevel().getServer());
    EasyNPC<?> defender = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 1));
    EasyNPC<?> factionMember = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));
    FactionHandler.setFaction(factionMember, factionName);

    LivingEntity outsider = spawnOutsideAttacker(helper, new Vec3(1, 2, 1));
    factionMember.getLivingEntity().setLastHurtByMob(outsider);

    CustomFactionHurtByTargetGoal<?> goal =
        new CustomFactionHurtByTargetGoal<>(defender, IMMEDIATE_SCAN_INTERVAL);
    GameTestHelpers.assertTrue(
        helper, "An NPC without a faction must not defend any faction", !goal.canUse());
  }

  public static void assertDefendSelfDoesNotAlertOthers(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> attackedNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));
    EasyNPC<?> bystanderNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 1));
    LivingEntity attacker = spawnOutsideAttacker(helper, new Vec3(1, 2, 1));

    helper.runAfterDelay(
        TICKS_BEFORE_ATTACK,
        () -> {
          attackedNPC.getLivingEntity().setLastHurtByMob(attacker);

          Goal defendSelfGoal =
              ObjectiveUtils.createObjectiveTarget(
                  new ObjectiveDataEntry(ObjectiveType.HURT_BY_TARGET), attackedNPC);
          GameTestHelpers.assertNotNull(helper, "Defend Self goal is null", defendSelfGoal);
          if (defendSelfGoal.canUse()) {
            defendSelfGoal.start();
          }

          GameTestHelpers.assertTrue(
              helper,
              "Defend Self must target the attacker",
              attackedNPC.getMob().getTarget() == attacker);
          GameTestHelpers.assertTrue(
              helper,
              "Defend Self must not pull uninvolved NPCs into the fight",
              bystanderNPC.getMob().getTarget() == null);
          helper.succeed();
        });
  }

  private static LivingEntity spawnOutsideAttacker(GameTestHelper helper, Vec3 position) {
    LivingEntity attacker = GameTestHelpers.spawnEntityType(helper, EntityType.ZOMBIE);
    attacker.setPos(helper.absoluteVec(position));
    return attacker;
  }

  private static void joinFaction(
      GameTestHelper helper, ServerPlayer serverPlayer, String factionName) {
    Scoreboard scoreboard = helper.getLevel().getScoreboard();
    PlayerTeam team = scoreboard.getPlayerTeam(factionName);
    if (team == null) {
      team = scoreboard.addPlayerTeam(factionName);
    }
    scoreboard.addPlayerToTeam(serverPlayer.getScoreboardName(), team);
  }
}
