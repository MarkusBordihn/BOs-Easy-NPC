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

package de.markusbordihn.easynpc.entity.easynpc.data;

import de.markusbordihn.easynpc.data.faction.FactionDataEntry;
import de.markusbordihn.easynpc.data.saveddata.FactionData;
import de.markusbordihn.easynpc.data.server.ServerDataAccessor;
import de.markusbordihn.easynpc.data.server.ServerDataIndex;
import de.markusbordihn.easynpc.data.server.ServerEntityData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.scores.PlayerTeam;
import net.minecraft.world.scores.Scoreboard;

public interface FactionDataCapable<T extends Mob> extends EasyNPC<T> {

  ServerDataAccessor<String> CUSTOM_DATA_FACTION_NAME =
      ServerEntityData.defineId(ServerDataIndex.FACTION_NAME, EntityDataSerializers.STRING);
  String DATA_FACTION_NAME_TAG = "FactionName";

  default String getFactionName() {
    return getEasyNPCServerData().getServerEntityData(CUSTOM_DATA_FACTION_NAME);
  }

  default void setFactionName(String factionName) {
    getEasyNPCServerData()
        .setServerEntityData(CUSTOM_DATA_FACTION_NAME, factionName != null ? factionName : "");
  }

  default boolean hasFactionName() {
    String factionName = this.getFactionName();
    return factionName != null && !factionName.isEmpty();
  }

  default void applyFactionToScoreboard() {
    ServerLevel serverLevel = this.getEntityServerLevel();
    if (serverLevel == null || this.getEntity() == null) {
      return;
    }

    Scoreboard scoreboard = serverLevel.getScoreboard();
    String scoreboardName = this.getEntity().getScoreboardName();
    String factionName = this.getFactionName();
    PlayerTeam currentTeam = scoreboard.getPlayersTeam(scoreboardName);

    if (factionName == null || factionName.isEmpty()) {
      if (currentTeam != null) {
        scoreboard.removePlayerFromTeam(scoreboardName, currentTeam);
      }
      return;
    }

    if (FactionData.isInitialized()) {
      FactionDataEntry factionDataEntry = FactionData.get().getFaction(factionName);
      if (factionDataEntry == null) {
        this.setFactionName("");
        if (currentTeam != null) {
          scoreboard.removePlayerFromTeam(scoreboardName, currentTeam);
        }
        return;
      }
    }

    PlayerTeam team = scoreboard.getPlayerTeam(factionName);
    if (team == null) {
      team = scoreboard.addPlayerTeam(factionName);
    }
    if (FactionData.isInitialized()) {
      FactionDataEntry factionDataEntry = FactionData.get().getFaction(factionName);
      if (factionDataEntry != null && factionDataEntry.getColor() != null) {
        team.setColor(factionDataEntry.getColor());
      }
    }

    if (currentTeam != null && factionName.equals(currentTeam.getName())) {
      return;
    }
    scoreboard.addPlayerToTeam(scoreboardName, team);
  }

  default void defineCustomFactionData() {
    getEasyNPCServerData().defineServerEntityData(CUSTOM_DATA_FACTION_NAME, "");
  }

  default void addAdditionalFactionData(CompoundTag compoundTag) {
    if (this.isServerSideInstance() && this.hasFactionName()) {
      compoundTag.putString(DATA_FACTION_NAME_TAG, this.getFactionName());
    }
  }

  default void readAdditionalFactionData(CompoundTag compoundTag) {
    if (compoundTag.contains(DATA_FACTION_NAME_TAG)) {
      this.setFactionName(compoundTag.getString(DATA_FACTION_NAME_TAG));
      if (this.isServerSideInstance()) {
        this.applyFactionToScoreboard();
      }
    }
  }
}
