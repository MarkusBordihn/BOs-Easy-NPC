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

package de.markusbordihn.easynpc.data.faction;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.StringTag;
import net.minecraft.world.scores.TeamColor;

public class FactionDataEntry {

  public static final String DATA_NAME_TAG = "Name";
  public static final String DATA_COLOR_TAG = "Color";
  public static final String DATA_HOSTILE_FACTIONS_TAG = "HostileFactions";

  private final String name;
  private final Set<String> hostileFactions = new HashSet<>();
  private TeamColor color;

  public FactionDataEntry(String name) {
    this.name = name;
  }

  public FactionDataEntry(CompoundTag compoundTag) {
    this.name = compoundTag.getString(DATA_NAME_TAG).orElse("");
    if (compoundTag.contains(DATA_COLOR_TAG)) {
      this.color = TeamColor.byName(compoundTag.getString(DATA_COLOR_TAG).orElse(null));
    }
    if (compoundTag.contains(DATA_HOSTILE_FACTIONS_TAG)) {
      ListTag hostileFactionsTag = compoundTag.getListOrEmpty(DATA_HOSTILE_FACTIONS_TAG);
      for (int i = 0; i < hostileFactionsTag.size(); i++) {
        this.hostileFactions.add(hostileFactionsTag.getString(i).orElse(""));
      }
    }
  }

  public String getName() {
    return this.name;
  }

  public TeamColor getColor() {
    return this.color;
  }

  public void setColor(TeamColor color) {
    this.color = color;
  }

  public Set<String> getHostileFactions() {
    return Collections.unmodifiableSet(this.hostileFactions);
  }

  public boolean addHostileFaction(String factionName) {
    return factionName != null && !factionName.isEmpty() && this.hostileFactions.add(factionName);
  }

  public boolean removeHostileFaction(String factionName) {
    return this.hostileFactions.remove(factionName);
  }

  public boolean isHostileTo(String factionName) {
    return factionName != null && this.hostileFactions.contains(factionName);
  }

  public CompoundTag createTag() {
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.putString(DATA_NAME_TAG, this.name);
    if (this.color != null) {
      compoundTag.putString(DATA_COLOR_TAG, this.color.getSerializedName());
    }
    if (!this.hostileFactions.isEmpty()) {
      ListTag hostileFactionsTag = new ListTag();
      for (String hostileFaction : this.hostileFactions) {
        hostileFactionsTag.add(StringTag.valueOf(hostileFaction));
      }
      compoundTag.put(DATA_HOSTILE_FACTIONS_TAG, hostileFactionsTag);
    }
    return compoundTag;
  }

  @Override
  public String toString() {
    return "FactionDataEntry [name="
        + this.name
        + ", color="
        + this.color
        + ", hostileFactions="
        + this.hostileFactions
        + ']';
  }
}
