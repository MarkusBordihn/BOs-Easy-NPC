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

package de.markusbordihn.easynpc.condition;

import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.RelationshipType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.FactionDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.handler.FactionHandler;
import java.util.Objects;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;

public class RelationshipCondition {

  private RelationshipCondition() {}

  public static boolean evaluate(
      ConditionDataEntry conditionDataEntry, Player player, LivingEntity npcContext) {
    if (conditionDataEntry == null
        || player == null
        || !(npcContext instanceof EasyNPC<?> easyNPC)) {
      return false;
    }

    if (!(conditionDataEntry.subType() instanceof RelationshipType relationshipType)) {
      return false;
    }

    String factionName = resolveFactionName(easyNPC, conditionDataEntry);
    return matches(
        relationshipType,
        isOwner(easyNPC, player),
        factionName,
        FactionHandler.getTargetGroupName(player),
        FactionHandler.isHostile(factionName, player));
  }

  public static boolean matches(
      RelationshipType relationshipType,
      boolean isOwner,
      String factionName,
      String playerGroupName,
      boolean isHostileToPlayer) {
    boolean hasFaction = factionName != null && !factionName.isEmpty();
    boolean isSameFaction = hasFaction && Objects.equals(factionName, playerGroupName);

    return switch (relationshipType) {
      case OWNER -> isOwner;
      case NOT_OWNER -> !isOwner;
      case SAME_FACTION -> isSameFaction;
      case NOT_SAME_FACTION -> !isSameFaction;
      case FRIENDLY_FACTION -> hasFaction && !isHostileToPlayer;
      case HOSTILE_FACTION -> hasFaction && isHostileToPlayer;
    };
  }

  /** Faction relations are server-side, so the client leaves final validation to the server. */
  public static boolean evaluateOnClient(
      ConditionDataEntry conditionDataEntry, Player player, LivingEntity npcContext) {
    if (conditionDataEntry == null
        || !(conditionDataEntry.subType() instanceof RelationshipType relationshipType)) {
      return true;
    }

    if (relationshipType == RelationshipType.FRIENDLY_FACTION
        || relationshipType == RelationshipType.HOSTILE_FACTION) {
      return true;
    }

    return evaluate(conditionDataEntry, player, npcContext);
  }

  private static boolean isOwner(EasyNPC<?> easyNPC, Player player) {
    OwnerDataCapable<?> ownerData = easyNPC.getEasyNPCOwnerData();
    return ownerData != null && ownerData.isNPCOwnedBy(player);
  }

  private static String resolveFactionName(
      EasyNPC<?> easyNPC, ConditionDataEntry conditionDataEntry) {
    if (conditionDataEntry.hasName()) {
      return conditionDataEntry.name();
    }

    FactionDataCapable<?> factionData = easyNPC.getEasyNPCFactionData();
    return factionData != null ? factionData.getFactionName() : null;
  }
}
