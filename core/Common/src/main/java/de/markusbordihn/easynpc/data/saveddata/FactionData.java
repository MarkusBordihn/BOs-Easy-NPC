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

package de.markusbordihn.easynpc.data.saveddata;

import com.mojang.serialization.Codec;
import com.mojang.serialization.DataResult;
import com.mojang.serialization.Dynamic;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.faction.FactionDataEntry;
import de.markusbordihn.easynpc.data.faction.FactionNameValidator;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.resources.Identifier;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.datafix.DataFixTypes;
import net.minecraft.world.level.saveddata.SavedData;
import net.minecraft.world.level.saveddata.SavedDataType;
import net.minecraft.world.scores.TeamColor;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class FactionData extends SavedData {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Identifier DATA_ID =
      Identifier.fromNamespaceAndPath(Constants.MOD_ID, "factions");
  private static final String DATA_FACTIONS_TAG = "Factions";
  private static final Codec<FactionData> CODEC =
      Codec.PASSTHROUGH.comapFlatMap(
          dynamic -> {
            try {
              CompoundTag tag = (CompoundTag) dynamic.convert(NbtOps.INSTANCE).getValue();
              return DataResult.success(loadFromNbt(tag));
            } catch (Exception e) {
              return DataResult.error(() -> "Failed to load FactionData: " + e.getMessage());
            }
          },
          data -> new Dynamic<>(NbtOps.INSTANCE, data.saveToNbt()));
  public static final SavedDataType<FactionData> TYPE =
      new SavedDataType<>(
          DATA_ID, FactionData::new, CODEC, DataFixTypes.SAVED_DATA_STRUCTURE_FEATURE_INDICES);
  private static FactionData instance;
  private final Map<String, FactionDataEntry> factions = new HashMap<>();

  public FactionData() {}

  private static FactionData loadFromNbt(CompoundTag compoundTag) {
    FactionData data = new FactionData();
    ListTag factionsTag = compoundTag.getListOrEmpty(DATA_FACTIONS_TAG);
    for (int i = 0; i < factionsTag.size(); i++) {
      FactionDataEntry factionDataEntry = new FactionDataEntry(factionsTag.getCompoundOrEmpty(i));
      if (!factionDataEntry.getName().isEmpty()) {
        data.factions.put(factionDataEntry.getName(), factionDataEntry);
      }
    }
    log.info("Loaded {} factions from faction registry", data.factions.size());
    return data;
  }

  public static FactionData get(MinecraftServer server) {
    if (server == null) {
      log.error("Cannot get FactionData: MinecraftServer is null");
      throw new IllegalArgumentException("MinecraftServer cannot be null");
    }

    return ServerSavedDataMigration.getOrMigrateFromOverworld(server, TYPE);
  }

  public static void init(MinecraftServer server) {
    instance = get(server);
  }

  public static boolean isInitialized() {
    return instance != null;
  }

  public static FactionData get() {
    if (instance == null) {
      throw new IllegalStateException("FactionData not initialized. Call init(server) first.");
    }
    return instance;
  }

  public boolean createFaction(String factionName) {
    if (!FactionNameValidator.isValid(factionName) || this.factions.containsKey(factionName)) {
      return false;
    }
    this.factions.put(factionName, new FactionDataEntry(factionName));
    this.setDirty();
    return true;
  }

  public boolean removeFaction(String factionName) {
    if (this.factions.remove(factionName) == null) {
      return false;
    }
    for (FactionDataEntry factionDataEntry : this.factions.values()) {
      factionDataEntry.removeHostileFaction(factionName);
    }
    this.setDirty();
    return true;
  }

  public FactionDataEntry getFaction(String factionName) {
    return factionName != null ? this.factions.get(factionName) : null;
  }

  public boolean hasFaction(String factionName) {
    return factionName != null && this.factions.containsKey(factionName);
  }

  public Set<String> getFactionNames() {
    return Collections.unmodifiableSet(new TreeSet<>(this.factions.keySet()));
  }

  public Collection<FactionDataEntry> getFactionEntries() {
    return Collections.unmodifiableCollection(this.factions.values());
  }

  public boolean setFactionColor(String factionName, TeamColor color) {
    FactionDataEntry factionDataEntry = this.getFaction(factionName);
    if (factionDataEntry == null || color == null) {
      return false;
    }
    factionDataEntry.setColor(color);
    this.setDirty();
    return true;
  }

  public boolean addHostileFaction(String factionName, String hostileFactionName) {
    FactionDataEntry factionDataEntry = this.getFaction(factionName);
    if (factionDataEntry == null || !factionDataEntry.addHostileFaction(hostileFactionName)) {
      return false;
    }
    this.setDirty();
    return true;
  }

  public boolean removeHostileFaction(String factionName, String hostileFactionName) {
    FactionDataEntry factionDataEntry = this.getFaction(factionName);
    if (factionDataEntry == null || !factionDataEntry.removeHostileFaction(hostileFactionName)) {
      return false;
    }
    this.setDirty();
    return true;
  }

  public boolean isHostile(String factionName, String targetName) {
    if (factionName == null
        || targetName == null
        || factionName.isEmpty()
        || targetName.isEmpty()
        || factionName.equals(targetName)) {
      return false;
    }
    FactionDataEntry factionDataEntry = this.factions.get(factionName);
    return factionDataEntry != null && factionDataEntry.isHostileTo(targetName);
  }

  private CompoundTag saveToNbt() {
    CompoundTag compoundTag = new CompoundTag();
    ListTag factionsTag = new ListTag();
    for (FactionDataEntry factionDataEntry : this.factions.values()) {
      factionsTag.add(factionDataEntry.createTag());
    }
    compoundTag.put(DATA_FACTIONS_TAG, factionsTag);
    return compoundTag;
  }
}
