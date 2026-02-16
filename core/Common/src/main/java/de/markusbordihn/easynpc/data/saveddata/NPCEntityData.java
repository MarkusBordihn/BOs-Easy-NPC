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
import de.markusbordihn.easynpc.data.npc.NPCEntityMetadata;
import de.markusbordihn.easynpc.data.npc.SavedNPCEntityEntry;
import de.markusbordihn.easynpc.data.storage.NPCFileStorage;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.resources.Identifier;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.datafix.DataFixTypes;
import net.minecraft.world.level.saveddata.SavedData;
import net.minecraft.world.level.saveddata.SavedDataType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class NPCEntityData extends SavedData {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String DATA_NAME = "easy_npc_index";
  private static final String DATA_METADATA_TAG = "Metadata";
  private static final String DATA_METADATA_UUID_TAG = "UUID";
  private static final String DATA_METADATA_DATA_TAG = "Data";

  private static final Codec<NPCEntityData> CODEC =
      Codec.PASSTHROUGH.comapFlatMap(
          dynamic -> {
            try {
              CompoundTag tag = (CompoundTag) dynamic.convert(NbtOps.INSTANCE).getValue();
              return DataResult.success(loadFromNbt(tag));
            } catch (Exception e) {
              return DataResult.error(() -> "Failed to load NPCEntityData: " + e.getMessage());
            }
          },
          data -> new Dynamic<>(NbtOps.INSTANCE, data.saveToNbt()));

  public static final SavedDataType<NPCEntityData> TYPE =
      new SavedDataType<>(
          DATA_NAME, NPCEntityData::new, CODEC, DataFixTypes.SAVED_DATA_STRUCTURE_FEATURE_INDICES);

  private final Map<UUID, NPCEntityMetadata> metadata = new HashMap<>();
  private final Map<UUID, Set<UUID>> entriesByOwner = new HashMap<>();
  private final Map<String, Set<UUID>> entriesByType = new HashMap<>();
  private final Map<String, Set<UUID>> entriesByDimension = new HashMap<>();
  private final Map<UUID, Set<UUID>> entriesByPreset = new HashMap<>();
  private final Map<Identifier, Set<UUID>> entriesByCustomIdentifier = new HashMap<>();
  private final Map<String, Set<UUID>> entriesByCustomIdentifierNamespace = new HashMap<>();

  private NPCFileStorage npcFileStorage;

  public NPCEntityData() {}

  private static NPCEntityData loadFromNbt(CompoundTag compoundTag) {
    NPCEntityData data = new NPCEntityData();
    ListTag metadataTag = compoundTag.getList(DATA_METADATA_TAG).orElse(new ListTag());
    for (int i = 0; i < metadataTag.size(); i++) {
      CompoundTag entryTag = metadataTag.getCompound(i).orElse(new CompoundTag());
      UUID uuid = CompoundTagUtils.readUUID(entryTag, DATA_METADATA_UUID_TAG);
      NPCEntityMetadata meta =
          NPCEntityMetadata.fromCompoundTag(
              entryTag.getCompound(DATA_METADATA_DATA_TAG).orElse(new CompoundTag()));
      if (uuid != null && meta != null) {
        data.metadata.put(uuid, meta);
        data.updateCachedMaps(uuid, meta);
      }
    }
    log.info("Loaded metadata for {} NPC entities from index", data.metadata.size());
    return data;
  }

  public static NPCEntityData get(MinecraftServer server) {
    if (server == null) {
      log.error("Cannot get NPCEntityData: MinecraftServer is null");
      throw new IllegalArgumentException("MinecraftServer cannot be null");
    }
    if (server.overworld() == null) {
      log.error("Cannot get NPCEntityData: Overworld is not yet loaded");
      throw new IllegalStateException("Overworld must be loaded before accessing NPCEntityData");
    }
    NPCEntityData data = server.overworld().getDataStorage().computeIfAbsent(TYPE);

    if (data.npcFileStorage == null) {
      data.npcFileStorage = new NPCFileStorage(Constants.WORLD_DIR);
    }

    return data;
  }

  public void putEntry(UUID uuid, SavedNPCEntityEntry entry) {
    if (uuid == null || entry == null) {
      log.warn("Attempted to put null UUID or entry");
      return;
    }

    NPCEntityMetadata oldMetadata = this.metadata.get(uuid);
    if (oldMetadata != null) {
      removeCachedMaps(uuid, oldMetadata);
    }

    this.metadata.put(uuid, entry.metadata());
    updateCachedMaps(uuid, entry.metadata());
    setDirty();

    if (npcFileStorage != null && entry.npcData() != null) {
      npcFileStorage.markDirty(uuid, entry.npcData());
    }
  }

  public void removeEntry(UUID uuid) {
    if (uuid == null) {
      return;
    }

    NPCEntityMetadata meta = this.metadata.remove(uuid);
    if (meta != null) {
      removeCachedMaps(uuid, meta);
      setDirty();
    }

    if (npcFileStorage != null) {
      npcFileStorage.delete(uuid);
    }
  }

  public Optional<SavedNPCEntityEntry> getEntry(UUID uuid) {
    if (uuid == null) {
      return Optional.empty();
    }

    NPCEntityMetadata meta = this.metadata.get(uuid);
    if (meta == null) {
      return Optional.empty();
    }

    if (npcFileStorage == null) {
      log.error("NPCFileStorage not initialized");
      return Optional.empty();
    }

    Optional<CompoundTag> npcData = npcFileStorage.load(uuid);
    if (npcData.isEmpty()) {
      log.warn("NPC file missing for UUID {}, removing from index", uuid);
      this.metadata.remove(uuid);
      removeCachedMaps(uuid, meta);
      setDirty();
      return Optional.empty();
    }

    return Optional.of(new SavedNPCEntityEntry(uuid, npcData.get(), meta));
  }

  public Collection<SavedNPCEntityEntry> getAllEntries() {
    if (npcFileStorage == null) {
      log.error("NPCFileStorage not initialized");
      return Collections.emptyList();
    }

    return this.metadata.keySet().stream()
        .map(this::getEntry)
        .filter(Optional::isPresent)
        .map(Optional::get)
        .collect(Collectors.toList());
  }

  public int getCount() {
    return this.metadata.size();
  }

  public Set<String> getAllEntityTypes() {
    return Collections.unmodifiableSet(entriesByType.keySet());
  }

  public Set<String> getAllDimensions() {
    return Collections.unmodifiableSet(entriesByDimension.keySet());
  }

  public Set<Identifier> getAllCustomIdentifiers() {
    return Collections.unmodifiableSet(entriesByCustomIdentifier.keySet());
  }

  public Set<String> getAllCustomIdentifierNamespaces() {
    return Collections.unmodifiableSet(entriesByCustomIdentifierNamespace.keySet());
  }

  public Collection<SavedNPCEntityEntry> getEntriesByOwner(UUID ownerUUID) {
    if (ownerUUID == null) {
      return Collections.emptyList();
    }
    Set<UUID> entityUUIDs = entriesByOwner.get(ownerUUID);
    if (entityUUIDs == null || entityUUIDs.isEmpty()) {
      return Collections.emptyList();
    }
    return entityUUIDs.stream()
        .map(this::getEntry)
        .filter(Optional::isPresent)
        .map(Optional::get)
        .collect(Collectors.toList());
  }

  public Collection<SavedNPCEntityEntry> getEntriesByType(String type) {
    if (type == null) {
      return Collections.emptyList();
    }
    Set<UUID> entityUUIDs = entriesByType.get(type);
    if (entityUUIDs == null || entityUUIDs.isEmpty()) {
      return Collections.emptyList();
    }
    return entityUUIDs.stream()
        .map(this::getEntry)
        .filter(Optional::isPresent)
        .map(Optional::get)
        .collect(Collectors.toList());
  }

  public Collection<SavedNPCEntityEntry> getEntriesByDimension(String dimension) {
    if (dimension == null) {
      return Collections.emptyList();
    }
    Set<UUID> entityUUIDs = entriesByDimension.get(dimension);
    if (entityUUIDs == null || entityUUIDs.isEmpty()) {
      return Collections.emptyList();
    }
    return entityUUIDs.stream()
        .map(this::getEntry)
        .filter(Optional::isPresent)
        .map(Optional::get)
        .collect(Collectors.toList());
  }

  public Collection<SavedNPCEntityEntry> getEntriesByPreset(UUID presetUUID) {
    if (presetUUID == null) {
      return Collections.emptyList();
    }
    Set<UUID> entityUUIDs = entriesByPreset.get(presetUUID);
    if (entityUUIDs == null || entityUUIDs.isEmpty()) {
      return Collections.emptyList();
    }
    return entityUUIDs.stream()
        .map(this::getEntry)
        .filter(Optional::isPresent)
        .map(Optional::get)
        .collect(Collectors.toList());
  }

  public Collection<SavedNPCEntityEntry> getEntriesByCustomIdentifier(Identifier customIdentifier) {
    if (customIdentifier == null) {
      return Collections.emptyList();
    }
    Set<UUID> entityUUIDs = entriesByCustomIdentifier.get(customIdentifier);
    if (entityUUIDs == null || entityUUIDs.isEmpty()) {
      return Collections.emptyList();
    }
    return entityUUIDs.stream()
        .map(this::getEntry)
        .filter(Optional::isPresent)
        .map(Optional::get)
        .collect(Collectors.toList());
  }

  public Collection<SavedNPCEntityEntry> getEntriesByCustomIdentifierNamespace(String namespace) {
    if (namespace == null) {
      return Collections.emptyList();
    }
    Set<UUID> entityUUIDs = entriesByCustomIdentifierNamespace.get(namespace);
    if (entityUUIDs == null || entityUUIDs.isEmpty()) {
      return Collections.emptyList();
    }
    return entityUUIDs.stream()
        .map(this::getEntry)
        .filter(Optional::isPresent)
        .map(Optional::get)
        .collect(Collectors.toList());
  }

  private void updateCachedMaps(UUID entityUUID, NPCEntityMetadata meta) {
    if (meta == null) {
      return;
    }

    if (meta.hasOwner()) {
      entriesByOwner.computeIfAbsent(meta.ownerUUID(), k -> new HashSet<>()).add(entityUUID);
    }

    if (meta.hasEntityType()) {
      entriesByType.computeIfAbsent(meta.entityType(), k -> new HashSet<>()).add(entityUUID);
    }

    if (meta.hasDimension()) {
      entriesByDimension.computeIfAbsent(meta.dimension(), k -> new HashSet<>()).add(entityUUID);
    }

    if (meta.hasPreset()) {
      entriesByPreset.computeIfAbsent(meta.presetUUID(), k -> new HashSet<>()).add(entityUUID);
    }

    if (meta.hasCustomIdentifier()) {
      Identifier customIdentifier = meta.customIdentifier();
      entriesByCustomIdentifier
          .computeIfAbsent(customIdentifier, k -> new HashSet<>())
          .add(entityUUID);
      entriesByCustomIdentifierNamespace
          .computeIfAbsent(customIdentifier.getNamespace(), k -> new HashSet<>())
          .add(entityUUID);
    }
  }

  private void removeCachedMaps(UUID entityUUID, NPCEntityMetadata meta) {
    if (meta == null) {
      return;
    }

    if (meta.hasOwner()) {
      Set<UUID> set = entriesByOwner.get(meta.ownerUUID());
      if (set != null) {
        set.remove(entityUUID);
        if (set.isEmpty()) {
          entriesByOwner.remove(meta.ownerUUID());
        }
      }
    }

    if (meta.hasEntityType()) {
      Set<UUID> set = entriesByType.get(meta.entityType());
      if (set != null) {
        set.remove(entityUUID);
        if (set.isEmpty()) {
          entriesByType.remove(meta.entityType());
        }
      }
    }

    if (meta.hasDimension()) {
      Set<UUID> set = entriesByDimension.get(meta.dimension());
      if (set != null) {
        set.remove(entityUUID);
        if (set.isEmpty()) {
          entriesByDimension.remove(meta.dimension());
        }
      }
    }

    if (meta.hasPreset()) {
      Set<UUID> set = entriesByPreset.get(meta.presetUUID());
      if (set != null) {
        set.remove(entityUUID);
        if (set.isEmpty()) {
          entriesByPreset.remove(meta.presetUUID());
        }
      }
    }

    if (meta.hasCustomIdentifier()) {
      Identifier customIdentifier = meta.customIdentifier();
      Set<UUID> set = entriesByCustomIdentifier.get(customIdentifier);
      if (set != null) {
        set.remove(entityUUID);
        if (set.isEmpty()) {
          entriesByCustomIdentifier.remove(customIdentifier);
        }
      }
      Set<UUID> namespaceSet =
          entriesByCustomIdentifierNamespace.get(customIdentifier.getNamespace());
      if (namespaceSet != null) {
        namespaceSet.remove(entityUUID);
        if (namespaceSet.isEmpty()) {
          entriesByCustomIdentifierNamespace.remove(customIdentifier.getNamespace());
        }
      }
    }
  }

  public int saveAllDirtyNPCs() {
    if (npcFileStorage != null) {
      return npcFileStorage.saveAllDirty();
    }
    return 0;
  }

  private CompoundTag saveToNbt() {
    CompoundTag compoundTag = new CompoundTag();
    ListTag metadataTag = new ListTag();
    for (Map.Entry<UUID, NPCEntityMetadata> entry : this.metadata.entrySet()) {
      CompoundTag entryTag = new CompoundTag();
      CompoundTagUtils.writeUUID(entryTag, DATA_METADATA_UUID_TAG, entry.getKey());
      entryTag.put(DATA_METADATA_DATA_TAG, entry.getValue().toCompoundTag());
      metadataTag.add(entryTag);
    }
    compoundTag.put(DATA_METADATA_TAG, metadataTag);

    int savedFiles = saveAllDirtyNPCs();
    if (savedFiles > 0) {
      log.debug(
          "Saved metadata for {} NPC entities to index and {} dirty NPC files",
          this.metadata.size(),
          savedFiles);
    } else {
      log.debug(
          "Saved metadata for {} NPC entities to index (no dirty files)", this.metadata.size());
    }

    return compoundTag;
  }
}
